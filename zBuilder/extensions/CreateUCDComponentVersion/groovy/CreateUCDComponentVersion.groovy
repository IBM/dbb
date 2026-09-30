@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript

import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.xml.MarkupBuilder
import groovy.json.JsonSlurper
import com.ibm.jzos.ZFile
import java.nio.file.*

/*
 * CreateUCDComponentVersion — zBuilder custom Groovy task
 *
 * Reads the DBB build report produced by the current build, generates a UCD
 * shiplist XML file, and invokes buztool.sh to create a new UCD component version.
 *
 * Required build context variables (set via dbb-build.yaml or dbb-app.yaml):
 *   UCD_BUZTOOL_PATH          — Absolute USS path to buztool.sh
 *   UCD_COMPONENT             — Name of the UCD component to create a version in
 *
 * Optional build context variables:
 *   UCD_VERSION_NAME          — Explicit version name (default: auto-assigned by UCD)
 *   UCD_BUZTOOL_PROPERTY_FILE — Absolute path to buztool property file (replaces -ar, UCD 7.1+)
 *   UCD_ARTIFACT_REPOSITORY   — Absolute path to artifact repository server connection file (deprecated)
 *   UCD_V2_PACKAGE_FORMAT     — Set to "true" to use createzosversion2 / UCD packaging format v2
 *   UCD_CONTAINER_MAPPING     — JSON map of last-level dataset qualifiers to deployType for v2 format
 *                               e.g. {"LOAD":"LOAD","DBRM":"DBRM","JCL":"TEXT"}
 *   UCD_PIPELINE_URL          — URL to the CI pipeline build result (added as component version property)
 *   UCD_PULL_REQUEST_URL      — URL to the pull/merge request (added as component version property)
 *   UCD_GIT_BRANCH            — Git branch name (added as component version property)
 *   UCD_GIT_COMMIT_URL_PREFIX — Git provider commit URL prefix for traceability links
 *   UCD_GIT_TREE_URL_PREFIX   — Git provider tree URL prefix for source traceability links
 *   UCD_PREVIEW               — Set to "true" to generate shiplist without invoking buztool.sh
 *
 * See CreateUCDComponentVersion.yaml for a sample variable configuration.
 */

// ─── Read required build context variables ────────────────────────────────────

String buztoolPath  = config.getStringVariable("UCD_BUZTOOL_PATH")
String component    = config.getStringVariable("UCD_COMPONENT")

assert buztoolPath : "Missing required build context variable: UCD_BUZTOOL_PATH"
assert component   : "Missing required build context variable: UCD_COMPONENT"

// ─── Read optional build context variables ────────────────────────────────────

String versionName          = config.getStringVariable("UCD_VERSION_NAME")
String buztoolPropertyFile  = config.getStringVariable("UCD_BUZTOOL_PROPERTY_FILE")
String artifactRepository   = config.getStringVariable("UCD_ARTIFACT_REPOSITORY")
String pipelineURL          = config.getStringVariable("UCD_PIPELINE_URL")
String pullRequestURL       = config.getStringVariable("UCD_PULL_REQUEST_URL")
String gitBranch            = config.getStringVariable("UCD_GIT_BRANCH")
String gitCommitURLPrefix   = config.getStringVariable("UCD_GIT_COMMIT_URL_PREFIX")
String gitTreeURLPrefix     = config.getStringVariable("UCD_GIT_TREE_URL_PREFIX")
String containerMappingJSON = config.getStringVariable("UCD_CONTAINER_MAPPING")
boolean ucdV2PackageFormat  = "true".equalsIgnoreCase(config.getStringVariable("UCD_V2_PACKAGE_FORMAT"))
boolean preview             = "true".equalsIgnoreCase(config.getStringVariable("UCD_PREVIEW"))

// ─── Resolve work directory from zBuilder build context ───────────────────────

String workDir = config.getStringVariable("BUILD_OUTPUT_DIR") ?: config.getStringVariable("workDir")
assert workDir : "Missing build context variable BUILD_OUTPUT_DIR (or workDir) — cannot locate build report"

String buildReportPath = "${workDir}/BuildReport.json"

// ─── Parse the build report ───────────────────────────────────────────────────

println "** CreateUCDComponentVersion: Reading build report ${buildReportPath}"
def buildReport = BuildReport.parse(new FileInputStream(buildReportPath))

/*
 * HashMap key: DeployableArtifact (member + deployType)
 * HashMap value: [container, buildReport, record, rank]
 * Last-write-wins for duplicates — rank is always 1 here (single report).
 */
Map<DeployableArtifact, List> tempBuildOutputsMap = [:]

def executeRecords = buildReport.getRecords().findAll {
    try {
        (it.getType() == DefaultRecordFactory.TYPE_EXECUTE || it.getType() == DefaultRecordFactory.TYPE_COPY_TO_PDS) &&
                !it.getOutputs().isEmpty()
    } catch (Exception e) { false }
}

// Drop outputs with no deployType or ZUNIT-TESTCASE
executeRecords.each {
    it.getOutputs().removeAll { o -> o.deployType == null || o.deployType == 'ZUNIT-TESTCASE' }
}

def ussRecords = buildReport.getRecords().findAll {
    try { it.getType() == "USS_RECORD" } catch (Exception e) { false }
}

def copyToUnixRecords = buildReport.getRecords().findAll {
    try { it.getType() == DefaultRecordFactory.TYPE_COPY_TO_UNIX && it.isOutput() } catch (Exception e) { false }
}

def deletions = buildReport.getRecords().findAll {
    try { it.getType() == "DELETE_RECORD" } catch (Exception e) { false }
}

executeRecords.each { rec ->
    rec.getOutputs().each { output ->
        def (ds, member) = getDatasetAndMember(output.dataset)
        tempBuildOutputsMap[new DeployableArtifact(member, output.deployType)] = [ds, buildReport, rec, 1]
    }
}

ussRecords.each { rec ->
    List<List<String>> outputs = []
    rec.getAttribute("outputs").split(';').each { entry ->
        outputs << entry.replaceAll('[\\[\\]]', '').split(',').toList()
    }
    outputs.each { output ->
        String rootDir = output[0].trim(); String file = output[1].trim(); String deployType = output[2].trim()
        tempBuildOutputsMap[new DeployableArtifact(file, deployType)] = [rootDir, buildReport, rec, 1]
    }
}

copyToUnixRecords.each { rec ->
    String targetPath = rec.getTargetPath(); String deployType = rec.getDeployType()
    if (deployType && deployType != 'ZUNIT-TESTCASE') {
        def targetFile = Paths.get(targetPath)
        tempBuildOutputsMap[new DeployableArtifact(targetFile.getFileName().toString(), deployType)] =
                [targetFile.getParent().toString(), buildReport, rec, 1]
    }
}

deletions.each { rec ->
    rec.getAttributeAsList("deletedBuildOutputs").each { deletedFile ->
        String cleansed = ((String) deletedFile).replace('"', '')
        tempBuildOutputsMap[new DeployableArtifact(cleansed, "DELETE")] = [deletedFile, buildReport, rec, 1]
    }
}

// Remove superseded execute/copy entries that have a later-ranked DELETE entry (and vice-versa)
Map<DeployableArtifact, List> buildOutputsMap = new HashMap<>(tempBuildOutputsMap)
tempBuildOutputsMap.each { artifact, info ->
    def rec = info[2]; def rank = info[3]
    if (rec.getType() == DefaultRecordFactory.TYPE_EXECUTE || rec.getType() == DefaultRecordFactory.TYPE_COPY_TO_PDS
            || rec.getType() == "USS_RECORD" || rec.getType() == DefaultRecordFactory.TYPE_COPY_TO_UNIX) {
        def container = info[0]
        def deleteKey = new DeployableArtifact(container + "(" + artifact.file + ")", "DELETE")
        if (tempBuildOutputsMap.containsKey(deleteKey)) {
            def deleteRank = tempBuildOutputsMap[deleteKey][3]
            if (rank > deleteRank) buildOutputsMap.remove(deleteKey)
            else                   buildOutputsMap.remove(artifact)
        }
    }
}

if (buildOutputsMap.isEmpty()) {
    println "** CreateUCDComponentVersion: No deployable outputs found in build report. Skipping."
    return 0
}

// ─── Generate shiplist XML ────────────────────────────────────────────────────

println "** CreateUCDComponentVersion: Generating UCD shiplist file"

def buildResult = buildReport.getRecords().find { it.getType() == DefaultRecordFactory.TYPE_BUILD_RESULT }
def buildResultRecord = buildReport.getRecords().find {
    try { it.getType() == DefaultRecordFactory.TYPE_PROPERTIES && it.getId() == "DBB.BuildResultProperties" } catch (Exception e) {}
}
def buildResultProperties = buildResultRecord?.getProperties()

def writer = new StringWriter()
writer.write("<?xml version=\"1.0\" encoding=\"CP037\"?>\n")
def xml = new MarkupBuilder(writer)

xml.manifest(type: "MANIFEST_SHIPLIST") {

    if (pipelineURL)   property(name: "ci-pipelineUrl",      value: pipelineURL)
    if (pullRequestURL) property(name: "ci-pullRequestURL",  value: pullRequestURL)
    if (gitBranch)     property(name: "ci-gitBranch",        value: gitBranch)

    if (buildResult != null) property(name: "dbb-buildResultUrl", label: buildResult.getLabel(), value: buildResult.getUrl())
    if (buildResultProperties != null) buildResultProperties.each { property(name: it.key, value: it.value) }

    buildOutputsMap.each { artifact, info ->
        def container = info[0]; def rec = info[2]

        if (rec.getType() == DefaultRecordFactory.TYPE_EXECUTE || rec.getType() == DefaultRecordFactory.TYPE_COPY_TO_PDS) {
            rec.getOutputs().each { output ->
                def fullDs = container + "(" + artifact.file + ")"
                if (fullDs == output.dataset && ZFile.exists("//'$container(${artifact.file})'")) {
                    println "   Shiplist entry: $container(${artifact.file}) deployType=${output.deployType}"
                    def githash = resolveGitHash(artifact.file, buildResultProperties, rec.getFile())
                    container(getContainerAttributes(container, ucdV2PackageFormat, containerMappingJSON)) {
                        resource(name: artifact.file, type: "PDSMember", deployType: output.deployType) {
                            property(name: "buildcommand", value: rec.getCommand())
                            if (rec.getType() == DefaultRecordFactory.TYPE_EXECUTE)
                                property(name: "buildoptions", value: rec.getOptions())
                            if (githash) {
                                property(name: "githash", value: githash)
                                if (gitCommitURLPrefix) property(name: "git-link-to-commit", value: "${gitCommitURLPrefix}/${githash}")
                            }
                            def inputUrl = (githash && gitTreeURLPrefix) ? "${gitTreeURLPrefix}/${githash}/${rec.getFile()}" : ""
                            inputs(url: inputUrl) {
                                input(name: rec.getFile(), compileType: "Main", url: inputUrl)
                                def depSets = buildReport.getRecords().findAll {
                                    it.getType() == DefaultRecordFactory.TYPE_DEPENDENCY_SET && it.getFile() == rec.getFile()
                                }
                                Set<String> seen = []
                                depSets.each { ds ->
                                    ds.getAllDependencies().each { dep ->
                                        if (dep.isResolved() && !seen.contains(dep.getLname()) && dep.getFile() != rec.getFile()) {
                                            def dUrl = (dep.getFile() && (dep.getCategory() == "COPY" || dep.getCategory() == "SQL INCLUDE") && githash && gitTreeURLPrefix) ?
                                                    "${gitTreeURLPrefix}/${githash}/${dep.getFile()}" : ""
                                            input(name: dep.getFile() ?: dep.getLname(), compileType: dep.getCategory(), url: dUrl)
                                            seen.add(dep.getLname())
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else if (!ZFile.exists("//'$container(${artifact.file})'")) {
                    println "*! $container(${artifact.file}) does not exist — skipped"
                }
            }
        } else if (rec.getType() == "USS_RECORD") {
            List<List<String>> outputs = []
            rec.getAttribute("outputs").split(';').each { entry ->
                outputs << entry.replaceAll('[\\[\\]]', '').split(',').toList()
            }
            outputs.each { output ->
                String rootDir = output[0].trim(); String file = output[1].trim(); String deployType = output[2].trim()
                if (artifact.file == file && Files.exists(Paths.get("${rootDir}/${file}"))) {
                    println "   Shiplist entry: ${rootDir}/${file} deployType=${deployType}"
                    def githash = resolveGitHash(artifact.file, buildResultProperties, rec.getAttribute("file"))
                    def (dir, relFile) = getDirectoryAndFile(file)
                    container(name: dir, rootDir: rootDir, type: "directory") {
                        resource(name: relFile, type: "file", deployType: deployType) {
                            property(name: "buildcommand", value: rec.getAttribute("command"))
                            property(name: "label",        value: rec.getAttribute("label"))
                            if (githash) {
                                property(name: "githash", value: githash)
                                if (gitCommitURLPrefix) property(name: "git-link-to-commit", value: "${gitCommitURLPrefix}/${githash}")
                            }
                            def inputUrl = (githash && gitTreeURLPrefix) ? "${gitTreeURLPrefix}/${githash}/${rec.getAttribute("file")}" : ""
                            inputs(url: inputUrl) { input(name: rec.getAttribute("file"), compileType: "Main", url: inputUrl) }
                        }
                    }
                } else if (!Files.exists(Paths.get("${rootDir}/${file}"))) {
                    println "*! ${rootDir}/${file} does not exist — skipped"
                }
            }
        } else if (rec.getType() == DefaultRecordFactory.TYPE_COPY_TO_UNIX) {
            String targetPath = rec.getTargetPath(); String deployType = rec.getDeployType()
            def targetFile = Paths.get(targetPath)
            if (Files.exists(targetFile)) {
                println "   Shiplist entry: ${targetPath} deployType=${deployType}"
                def githash = resolveGitHash(artifact.file, buildResultProperties, rec.getSourcePath())
                container(name: targetFile.getParent().toString(), rootDir: targetFile.getParent().toString(), type: "directory") {
                    resource(name: targetFile.getFileName().toString(), type: "file", deployType: deployType) {
                        if (githash) {
                            property(name: "githash", value: githash)
                            if (gitCommitURLPrefix) property(name: "git-link-to-commit", value: "${gitCommitURLPrefix}/${githash}")
                        }
                        def inputUrl = (githash && gitTreeURLPrefix) ? "${gitTreeURLPrefix}/${githash}/${rec.getSourcePath()}" : ""
                        inputs(url: inputUrl) { input(name: rec.getSourcePath(), compileType: "Main", url: inputUrl) }
                    }
                }
            } else {
                println "*! ${targetPath} does not exist — skipped"
            }
        } else if (rec.getType() == "DELETE_RECORD") {
            rec.getAttributeAsList("deletedBuildOutputs").each { deletedOutput ->
                String cleansed = ((String) deletedOutput).replace('"', '')
                if (artifact.file == cleansed) {
                    println "   Shiplist delete entry: ${cleansed}"
                    def (ds, member) = getDatasetAndMember(cleansed)
                    deleted {
                        container(getContainerAttributes(ds, ucdV2PackageFormat, containerMappingJSON)) {
                            resource(name: member, type: "PDSMember")
                        }
                    }
                }
            }
        }
    }
}

def shiplistFile = new File("${workDir}/shiplist.xml")
shiplistFile.text = writer.toString()
println "** CreateUCDComponentVersion: Shiplist written to ${workDir}/shiplist.xml"

// ─── Invoke buztool.sh ────────────────────────────────────────────────────────

def buztoolCmd = ucdV2PackageFormat ? "createzosversion2" : "createzosversion"
def cmd = [buztoolPath, buztoolCmd, "-c", component, "-s", "${workDir}/shiplist.xml", "-o", "${workDir}/buztool.output"]
if (artifactRepository)   { cmd << "-ar";   cmd << artifactRepository }
if (buztoolPropertyFile)  { cmd << "-prop"; cmd << buztoolPropertyFile }
if (versionName)          { cmd << "-v";    cmd << "\"${versionName}\"" }

println "** CreateUCDComponentVersion: buztool command: ${cmd.join(' ')}"

if (preview) {
    println "** CreateUCDComponentVersion: Preview mode — buztool.sh not invoked."
    return 0
}

StringBuffer stdout = new StringBuffer(); StringBuffer stderr = new StringBuffer()
def proc = cmd.execute()
proc.waitForProcessOutput(stdout, stderr)
println stdout.toString()

def rc = proc.exitValue()
if (rc == 0) {
    println "** CreateUCDComponentVersion: buztool output properties:"
    def outProps = new Properties()
    new File("${workDir}/buztool.output").withInputStream { outProps.load(it) }
    outProps.each { k, v -> println "   $k -> $v" }
} else {
    println "*! CreateUCDComponentVersion: buztool.sh failed (rc=${rc})\n${stderr}"
}
return rc

// ─── Helper methods ───────────────────────────────────────────────────────────

def getDatasetAndMember(String fullname) {
    def elements = fullname.split("[\\(\\)]")
    return [elements[0], elements.size() > 1 ? elements[1] : ""]
}

def getDirectoryAndFile(String fullname) {
    def p = Paths.get(fullname)
    return [p.getParent()?.toString() ?: ".", p.getFileName().toString()]
}

def getContainerAttributes(String ds, boolean v2, String mappingJSON) {
    if (v2) {
        String lastQual = ds.tokenize('.').last()
        String deployType = lastQual
        if (mappingJSON) {
            def mapping = new JsonSlurper().parseText(mappingJSON)
            deployType = mapping[lastQual] ?: lastQual
        }
        return [name: ds, type: "PDS", deployType: deployType]
    }
    return [name: ds, type: "PDS"]
}

def resolveGitHash(String memberName, def buildResultProperties, String sourceFile) {
    if (buildResultProperties == null || sourceFile == null) return ""
    def prop = buildResultProperties.find { it.key.contains(":githash:") && sourceFile.contains(it.key.substring(9)) }
    return prop?.getValue() ?: ""
}
