@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import java.io.File
import com.ibm.dbb.build.CopyToHFS
import com.ibm.dbb.build.BuildProperties
import com.ibm.dbb.build.DBBConstants
import com.ibm.dbb.build.DBBConstants.CopyMode
import com.ibm.dbb.build.report.BuildReport
import com.ibm.dbb.build.report.records.*
import groovy.transform.*
import groovy.cli.commons.*
import java.nio.file.*
import static java.nio.file.StandardCopyOption.*
import com.ibm.jzos.ZFile;

/************************************************************************************
 * This script creates a simplified package with the outputs generated from a DBB build
 * Optionally, it publishes it to an Artifact repository.
 *
 * usage: see help command or README
 *
 * Version 0 - 2019
 *  called PublishLoadModule.groovy and located in Build/PublishLoadModules
 *
 * Version 1 - 2021
 *  Re-Design to run as a post-build script and make publishing optional
 *
 * Version 2 - 2022-02
 *  - Externalize the Map of LLQ to CopyMode
 *  - Add capablity to add additional files from build workspace
 *  - Verbose logging will print tar contents
 *
 * Version 3 - 2022-08
 *  - Ability to pass multiple build reports to build a cumulative package
 *  - Add an optional option to add the deploy type extension to the member
 *
 * Version 4 - 2022-12
 *  - Generalized the options to publish package to artifact repository
 *  - Script is supporting both JFrog Artifactory and SonarType Nexus
 *  - Added additional CLI option to pass artifact repository parameters
 *
 * Version 5 - 2023-07
 *  - Added support for DeployableArtifact, to manage the correct stacking
 *    of duplicates artifacts
 * 
 * Version 6 - 2024-03
 *  - Added support to write IBM Wazi Deploy Application Manifest file
 *      
 * Version 7 - 2024-04
 *  - Added support to SBOM files
 *      
 * Version 8 - 2024-07
 *  - Reworked error management and fixed few glitches
 *      
 ************************************************************************************/

// start create & publish package
@Field Properties props = null
def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
@Field def wdManifestGeneratorUtilities = loadScript(new File("${scriptDir}/utilities/WaziDeployManifestGenerator.groovy"))
@Field def sbomUtilities
@Field def rc = 0

def startTime = new Date()

props = parseInput(args)

// Just printing the Usage and exiting
if (rc == 1) {
	System.exit(0)	
}
props.startTime = startTime.format("yyyyMMdd.HHmmss.SSS")
println("** PackageBuildOutputs start at $props.startTime")
println("** Properties at startup:")
props.sort().each { k,v->
	if ( k == "artifactRepository.password" )
		println "   $k -> xxxxxx "
	else
		println "   $k -> $v"
}
if (rc > 1) {
	println("*! [ERROR] One or several properties were missing in the configuration. Review the console output.")
	System.exit(rc)
}


// Enable file tagging
BuildProperties.setProperty("dbb.file.tagging", "true") // Enable dbb file tagging

// Map of last level dataset qualifier to DBB CopyToHFS CopyMode.
def copyModeMap = parseCopyModeMap(props.copyModeMap)


// Hashmap of BuildOutput to Record
Map<DeployableArtifact, Map> buildOutputsMap = new HashMap<DeployableArtifact, Map>()

// Field to store default tarFileLabel (buildInfo.label) when cli argument tarFileName is not passed.
def String tarFileLabel = "Default"

// Object to store scm information for Wazi Deploy Application Manifest file
HashMap<String,String> scmInfo = new HashMap<String, String>()

if (props.generateSBOM && props.generateSBOM.toBoolean()) {
	sbomUtilities = loadScript(new File("${scriptDir}/utilities/sbomGenerator.groovy"))
	sbomUtilities.initializeSBOM(props.sbomAuthor)
}

// iterate over all build reports to obtain build output
props.buildReportOrder.each { buildReportFile ->
	println("** Read build report data from '${buildReportFile}'.")
	def jsonOutputFile = new File(buildReportFile)

	if (!jsonOutputFile.exists()){
		println("*! [ERROR] Build Report '$buildReportFile' not found.")
		rc = 1
	} else {
		def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))
	
		// Read buildInfo to obtain build information
		def buildInfo = buildReport.getRecords().findAll{
			try {
				it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT
			} catch (Exception e){}
		}
		if (buildInfo.size() != 0) {
			tarFileLabel = buildInfo[0].label
		}
	
		// retrieve the buildResultPropertiesRecord
		def buildResultPropertiesRecord = buildReport.getRecords().find {
			try {
				it.getType()==DefaultRecordFactory.TYPE_PROPERTIES && it.getId()=="DBB.BuildResultProperties"
			} catch (Exception e){}
		}
	
		// finds all the build outputs with a deployType
		def buildRecords = buildReport.getRecords().findAll{
			try {
				(it.getType()==DefaultRecordFactory.TYPE_EXECUTE || it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS) &&
						!it.getOutputs().isEmpty()
			} catch (Exception e){}
		}
	
		// finds all the build outputs with a deployType
		// Today the USS_RECORD type is built using an AnyTypeRecord record
		// An Idea is currently opened to have an official USS_RECORD: https://ideas.ibm.com/ideas/DBB-I-43
		def ussBuildRecords = buildReport.getRecords().findAll{
			try {
				it.getType()=="USS_RECORD" && !it.getAttribute("outputs").isEmpty()
			} catch (Exception e){}
		}
	
		if (props.deployTypeFilter){
			println("** Filter Output Records on following deployTypes: ${props.deployTypeFilter}...")
			buildRecords.each {
				// filtered executes
				def filteredOutputs =  it.getOutputs().findAll{ o ->
					o.deployType != null && (props.deployTypeFilter).split(',').contains(o.deployType)
				}
				// Manipulating the scope of build outputs
				it.getOutputs().clear()
				it.getOutputs().addAll(filteredOutputs)
			}
			ussBuildRecords.each {
				ArrayList<ArrayList> outputs = []
				it.getAttribute("outputs").split(';').collectEntries { entry ->
					outputs += entry.replaceAll('\\[|\\]', '').split(',')
				}
	
				ArrayList<String> filteredOutputs = []
				outputs.each{ output ->
					rootDir = output[0].trim()
					file = output[1].trim()
					deployType = output[2].trim()
					if (!(props.deployTypeFilter).split(',').contains(deployType)) {
						filteredOutputs += output.toString()
					}
				}
	
				def filteredOutputsStrings = String.join(";", filteredOutputs)
				it.setAttribute("outputs", filteredOutputsStrings)
			}
		} else {
			// Remove outputs without deployType + ZUNIT-TESTCASEs
			println("** Remove output records without deployType or with deployType=ZUNIT-TESTCASE")
			buildRecords.each {
				def unwantedOutputs =  it.getOutputs().findAll{ o ->
					o.deployType == null || o.deployType == 'ZUNIT-TESTCASE'
				}
				it.getOutputs().removeAll(unwantedOutputs)
			}
		}
	
		buildRecords += ussBuildRecords
	
		def datasetMembersCount = 0
		def zFSFilesCount = 0
	
		// adding files and executes with outputs to Hashmap to remove redundant data
		buildRecords.each{ buildRecord ->
			if (buildRecord.getType()=="USS_RECORD") {
				if (!buildRecord.getAttribute("outputs").isEmpty()) {
					ArrayList<ArrayList> outputs = []
					buildRecord.getAttribute("outputs").split(';').collectEntries { entry ->
						outputs += entry.replaceAll('\\[|\\]', '').split(',')
					}
					zFSFilesCount += outputs.size()
					outputs.each{ output ->
						rootDir = output[0].trim()
						file = output[1].trim()
						deployType = output[2].trim()
						def dependencySetRecord = buildReport.getRecords().find {
							it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET && it.getFile().equals(file)
						}
						buildOutputsMap.put(new DeployableArtifact(file, deployType, "zFSFile"), [
							container: rootDir,
							owningApplication: props.application,
							record: buildRecord,
							propertiesRecord: buildResultPropertiesRecord,
							dependencySetRecord: dependencySetRecord
						])
					}
				}
			} else {
				if (buildRecord.getOutputs().size() != 0) {
					buildRecord.getOutputs().each{ output ->
						datasetMembersCount++
						def (dataset, member) = getDatasetName(output.dataset)
						String file = buildRecord.getFile()
						def dependencySetRecord = buildReport.getRecords().find {
							it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET && it.getFile().equals(file)
						}
						buildOutputsMap.put(new DeployableArtifact(member, output.deployType, "DatasetMember"), [
							container: dataset,
							owningApplication: props.application,
							record: buildRecord,
							propertiesRecord: buildResultPropertiesRecord,
							dependencySetRecord: dependencySetRecord
						])
					}
				}
			}
		}
	
		if ( datasetMembersCount + zFSFilesCount == 0 ) {
			println("** No items to package in '$buildReportFile'.")
		} else {
			println("** Deployable artifacts detected in '$buildReportFile':")
			buildRecords.each { record ->
				if (record.getType()=="USS_RECORD") {
					if (!record.getAttribute("outputs").isEmpty()) {
						ArrayList<ArrayList> outputs = []
						record.getAttribute("outputs").split(';').collectEntries { entry ->
							outputs += entry.replaceAll('\\[|\\]', '').split(',')
						}
						outputs.each{ output ->
							rootDir = output[0].trim()
							file = output[1].trim()
							deployType = output[2].trim()
							println("   $rootDir/$file, $deployType")
						}
					}
				} else {
					record.getOutputs().each {println("   ${it.dataset}, ${it.deployType}")}
				}
			}
		}
	
		// generate scmInfo for Wazi Deploy Application Manifest file
		if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
			if (props.buildReportOrder.size() == 1) {
				scmInfo.put("type", "git")
				gitUrl = retrieveBuildResultProperty (buildResultPropertiesRecord, "giturl")
				if (gitUrl) scmInfo.put("uri", gitUrl)
				gitHash = retrieveBuildResultProperty (buildResultPropertiesRecord, "githash")
				if (gitHash) scmInfo.put("shortCommit", gitHash)
				scmInfo.put("branch", props.branch)
			} else {
				scmInfo.put("shortCommit", "multipleBuildReports")
				scmInfo.put("uri", "multipleBuildReports")
			}
		}
	}
}

if (rc == 0) {

	if (buildOutputsMap.size() == 0) {
		println("** There are no build outputs found in all provided build reports. Exiting.")
		rc = 0
	} else {
	
		// Local variables
		// Initialize Wazi Deploy Manifest Generator
		if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
			wdManifestGeneratorUtilities.initWaziDeployManifestGenerator(props)// Wazi Deploy Application Manifest
			wdManifestGeneratorUtilities.setScmInfo(scmInfo)
		}
		def String tarFileName = (props.tarFileName) ? props.tarFileName  : "${tarFileLabel}.tar"
		def tarFile = "$props.workDir/${tarFileName}"
	
		//Create a temporary directory on zFS to copy the load modules from data sets to
		def tempLoadDir = new File("$props.workDir/tempPackageDir")
		!tempLoadDir.exists() ?: tempLoadDir.deleteDir()
		tempLoadDir.mkdirs()
	
		println("*** Number of build outputs to package: ${buildOutputsMap.size()}")
	
		println("** Copy build outputs to temporary package directory '$tempLoadDir'")
	
		buildOutputsMap.each { deployableArtifact, info ->
			String container = info.get("container")
			String owningApplication = info.get("owningApplication")
			Record record = info.get("record")
			PropertiesRecord propertiesRecord = info.get("propertiesRecord")
			DependencySetRecord dependencySetRecord = info.get("dependencySetRecord")
			
			def filePath = ""
			if (deployableArtifact.artifactType.equals("zFSFile")) {
				filePath = "$tempLoadDir"
			} else {
				filePath = "$tempLoadDir/$container"
			}
	
			// define file name in USS
			def fileName = deployableArtifact.file
	
			// add deployType to file name
			if (props.addExtension && props.addExtension.toBoolean()) {
				fileName = fileName + '.' + deployableArtifact.deployType
			}
			def file = new File(filePath, fileName)
	
			def (directory, relativeFileName) = extractDirectoryAndFile(file.toPath().toString())
			new File(directory).mkdirs()
	
	
			if (deployableArtifact.artifactType.equals("zFSFile")) {
				def originalFile = new File(container + "/" + deployableArtifact.file)
				println "   Copy '${originalFile.toPath()}' to '${file.toPath()}'"
				try {
					Files.copy(originalFile.toPath(), file.toPath(), StandardCopyOption.COPY_ATTRIBUTES);
				} catch (IOException exception) {
					println "!* [ERROR] Copy failed: an error occurred when copying '${originalFile.toPath()}' to '${file.toPath()}'"
					rc = Math.max(rc, 1) 
				}
			} else {
				// set copyMode based on last level qualifier
				currentCopyMode = copyModeMap[container.replaceAll(/.*\.([^.]*)/, "\$1")]
				if (currentCopyMode != null) {
					if (ZFile.exists("//'$container(${deployableArtifact.file})'")) {
						// Copy outputs to HFS
						CopyToHFS copy = new CopyToHFS()
						copy.setCopyMode(DBBConstants.CopyMode.valueOf(currentCopyMode))
						copy.setDataset(container)
	
						println "   Copy '$container(${deployableArtifact.file})' to '$filePath/$fileName' with DBB Copymode '$currentCopyMode'"
						copy.dataset(container).member(deployableArtifact.file).file(file).execute()
	
						// Tagging binary files
						if (currentCopyMode == CopyMode.BINARY || currentCopyMode == CopyMode.LOAD) {
							StringBuffer stdout = new StringBuffer()
							StringBuffer stderr = new StringBuffer()
							Process process = "chtag -b $file".execute()
							process.waitForProcessOutput(stdout, stderr)
							if (stderr){
								println ("*! stderr : $stderr")
								println ("*! stdout : $stdout")
							}
						}
	
						// Append record to Wazi Deploy Application Manifest
						if (wdManifestGeneratorUtilities && props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
							wdManifestGeneratorUtilities.appendArtifactToAppManifest(deployableArtifact, "$container/$fileName", record, propertiesRecord)
						}
	
					} else {
						println "*! [ERROR] Copy failed: The file '$container(${deployableArtifact.file})' doesn't exist."
						rc = Math.max(rc, 1) 
					}
				} else {
					println "*! [ERROR] Copy failed: The file '$container(${deployableArtifact.file})' could not be copied due to missing mapping."
					rc = Math.max(rc, 1) 
				}
			}
			if (props.generateSBOM && props.generateSBOM.toBoolean() && rc == 0) {
				sbomUtilities.addEntryToSBOM(deployableArtifact, info)
			}
		}
		
		if (props.generateSBOM && props.generateSBOM.toBoolean() && rc == 0) {
			sbomUtilities.writeSBOM("$tempLoadDir/sbom.json", props.fileEncoding)    
		}
		
	
		if (wdManifestGeneratorUtilities && props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean() && rc == 0) {
			// print application manifest
			// wazideploy_manifest.yml is the default name of the manifest file
			wdManifestGeneratorUtilities.writeApplicationManifest(new File("$tempLoadDir/wazideploy_manifest.yml"), props.fileEncoding, props.verbose)
		}
	
		if (rc == 0) {
	
			// log buildReportOrder file and add build reports to tar file
			File buildReportOrder = new File("$tempLoadDir/buildReportOrder.txt")
	
			println("** Generate package build report order file to '$buildReportOrder'")
	
			buildReportOrder.write('')
			String buildReportFileName
			int counter = 0
	
			buildReportOrder.withWriter(props.fileEncoding) { writer ->
				props.buildReportOrder.each{ buildReportFile ->
					counter++
	
					Path buildReportFilePath = Paths.get(buildReportFile)
					Path copiedBuildReportFilePath = Paths.get(tempLoadDir.getPath() + "/" + buildReportFilePath.getFileName().toString())
	
					// prefixing the buildreport with sequence number when having multiple
					if (props.buildReportOrder.size() > 1)
						copiedBuildReportFilePath = Paths.get(tempLoadDir.getPath() + "/" + "$counter".padLeft(3, "0") + "_" + buildReportFilePath.getFileName().toString())
	
					Files.copy(buildReportFilePath, copiedBuildReportFilePath, COPY_ATTRIBUTES)
					writer.write("${copiedBuildReportFilePath.toString()}\n")
				}
			}
		
			Path packagingPropertiesFilePath = Paths.get(props.packagingPropertiesFile)
			Path copiedPackagingPropertiesFilePath = Paths.get(tempLoadDir.getPath() + "/" + packagingPropertiesFilePath.getFileName().toString())
			if(props.verbose) println("** Copy packaging properties config file to '$copiedPackagingPropertiesFilePath'")
			Files.copy(packagingPropertiesFilePath, copiedPackagingPropertiesFilePath, COPY_ATTRIBUTES)
	
			println("** Create tar file at ${tarFile}")
			// Note: https://www.ibm.com/docs/en/zos/2.4.0?topic=scd-tar-manipulate-tar-archive-files-copy-back-up-file
			// To save all attributes to be restored on z/OS and non-z/OS systems : tar -UX
			def processCmd = [
				"sh",
				"-c",
				"tar cUXf $tarFile *"
			]
	
			def processRC = runProcess(processCmd, tempLoadDir)
			rc = Math.max(rc, processRC)
			if (rc == 0) {
				println("** Package '${tarFile}' successfully created.")
			} else {
				println("*! [ERROR] Error when creating Package '${tarFile}' with rc=$rc.")
			}
		}
	
		//Package additional outputs to tar file.
		if (props.includeLogs && rc == 0) (props.includeLogs).split(",").each { logPattern ->
			println("** Add files with file pattern '$logPattern' from '${props.workDir}' to '${tarFile}'")
			processCmd = [
				"sh",
				"-c",
				"tar rUXf $tarFile $logPattern"
			]
	
			processRC = runProcess(processCmd, new File(props.workDir))
			rc = Math.max(rc, processRC)
			if (rc != 0) {
				println("*! [ERROR] Error when appending '$logPattern' files to Package '${tarFile}' with rc=$rc.")
			}
		}
	
		if (props.verbose && props.verbose.toBoolean() && rc  == 0) {
			println ("** List package contents.")
	
			processCmd = [
				"sh",
				"-c",
				"tar tvf $tarFile"
			]
	
			processRC = runProcess(processCmd, new File(props.workDir))
			rc = Math.max(rc, processRC)
			if (rc != 0) {
				println("*! [ERROR] Error when listing contents of Package '${tarFile}' with rc=$rc.")
			}
		}
	
		//Set up the artifact repository information to publish the tar file
		if (props.publish && props.publish.toBoolean() && rc == 0){
			// Configuring artifact repositoryHelper parms
			def String remotePath = (props.versionName) ? (props.versionName + "/" + tarFileName) : (tarFileLabel + "/" + tarFileName)
			def url = new URI(props.get('artifactRepository.url') + "/" + props.get('artifactRepository.repo') + "/" + props.'artifactRepository.directory' + "/" + remotePath ).normalize().toString() // Normalized URL
	
			def apiKey = props.'artifactRepository.user'
			def user = props.'artifactRepository.user'
			def password = props.'artifactRepository.password'
			def httpClientVersion = props.'artifactRepository.httpClientVersion'
			def repo = props.get('artifactRepository.repo') as String
	
			//Call the artifactRepositoryHelpers to publish the tar file
			File artifactRepoHelpersFile = new File("$scriptDir/ArtifactRepositoryHelpers.groovy")
			Class artifactRepositoryHelpersClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(artifactRepoHelpersFile)
			GroovyObject artifactRepositoryHelpers = (GroovyObject) artifactRepositoryHelpersClass.newInstance()
	
			println ("** Upload package to Artifact Repository '$url'.")
			artifactRepositoryHelpers.upload(url, tarFile as String, user, password, props.verbose.toBoolean(), httpClientVersion)
		}
	}
}

if (rc > 0) {
	println ("*! [ERROR] Packaging failed with rc=$rc. Review the console output.")
} else {
	println ("** Packaging completed successfully.")
}
System.exit(rc)

/**
 * parse data set name and member name
 * @param fullname e.g. BLD.LOAD(PGM1)
 * @return e.g. (BLD.LOAD, PGM1)
 */
def getDatasetName(String fullname){
	def ds,member;
	def elements =  fullname.split("[\\(\\)]");
	ds = elements[0];
	member = elements.size()>1? elements[1] : "";
	return [ds, member];
}

/**
 * Extract the directory and the file 
 * from the fullname of a zFS file
 * For instance: /var/test/file.txt  --> [/var/test, file.txt]
 */
def extractDirectoryAndFile(String fullname) {
	Path filePath = Paths.get(fullname);
	String file = filePath.getFileName().toString();
	String directory = filePath.getParent();
	return [directory, file];
}


/**
 * run process
 */
def runProcess(ArrayList cmd, File dir){
	if (props.verbose && props.verbose.toBoolean()) println "   Executing $cmd "
	StringBuffer response = new StringBuffer()
	StringBuffer error = new StringBuffer()

	// execute cmd
	def p = cmd.execute(null, dir)

	p.waitForProcessOutput(response, error)
	if(response) println(response.toString())

	def rc = p.exitValue();
	if (rc != 0){
		println("*! [ERROR] Error executing $cmd \n" + error.toString())
	}
	return rc
}

/**
 * read cliArgs
 */
def parseInput(String[] cliArgs){
	def cli = new CliBuilder(usage: "PackageBuildOutputs.groovy [options]", stopAtNonOption:false)
	// required packaging options
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the DBB build output directory')
	cli.properties(longOpt:'packagingPropertiesFile', args:1, argName:'packagingPropertiesFile', 'Path of a property file containing application specific packaging details.')

	// optional packaging options
	cli.d(longOpt:'deployTypes', args:1, argName:'deployTypes','Comma-seperated list of deployTypes to filter on the scope of the tar file. (Optional)')
	cli.t(longOpt:'tarFileName', args:1, argName:'filename', 'Name of the package tar file. (Optional unless using --buildReportOrder or --buildReportOrderFile)')
	cli.il(longOpt:'includeLogs', args:1, argName:'includeLogs', 'Comma-separated list of files/patterns from the USS build workspace. (Optional)')
	cli.ae(longOpt:'addExtension', 'Flag to add the deploy type extension to the member in the package tar file. (Optional)')

	// Wazi Deploy Application Manifest generation
	cli.wd(longOpt:'generateWaziDeployAppManifest', 'Flag indicating to generate and add the Wazi Deploy Application Manifest file.')

	cli.b(longOpt:'branch', args:1, argName:'branch', 'The git branch processed by the pipeline')
	cli.a(longOpt:'application', args:1, argName:'application', 'The name of the application')
	
	// Artifact repository options ::
	cli.p(longOpt:'publish', 'Flag to indicate package upload to the provided Artifact Repository server. (Optional)')
	cli.v(longOpt:'versionName', args:1, argName:'versionName', 'Name of the version/package on the Artifact repository server. (Optional)')

	// Artifact repository info
	cli.au(longOpt:'artifactRepositoryUrl', args:1, argName:'url', 'URL to the Artifact repository server. (Optional)')
	cli.ar(longOpt:'artifactRepositoryName', args:1, argName:'repoName', 'Artifact repository name to store the build. (Optional)')
	cli.ad(longOpt:'artifactRepositoryDirectory', args:1, argName:'repoDirectory', 'Directory path in the repository to store the build . (Optional)')
	cli.aU(longOpt:'artifactRepositoryUser', args:1, argName:'user', 'User to connect to the Artifact repository server. (Optional)')
	cli.aP(longOpt:'artifactRepositoryPassword', args:1, argName:'password', 'Password to connect to the Artifact repository server. (Optional)')
	cli.ah(longOpt:'artifactRepositoryHttpClientProtocolVersion', args:1, argName: 'httpClientProtocolVersion', 'HttpClient.Version setting to override the HTTP protocol version. (Optional)')
	cli.aprop(longOpt:'artifactRepositoryPropertyFile', args:1, argName:'propertyFile', 'Path of a property file containing application specific artifact repository details. (Optional) ** (Deprecated)')

	// Tracing
	cli.verb(longOpt:'verbose', 'Flag to provide more log output. (Optional)')

	// multiple build reports
	cli.boFile(longOpt:'buildReportOrderFile', args:1, argName:'buildReportOrderFile', 'A file that lists build reports in order of processing')
	cli.bO(longOpt:'buildReportOrder', args:1, argName:'buildReportOrder', 'List of build reports in order of processing ')

	// SBOM generation
	cli.s(longOpt:'sbom', argName:'sbom', 'Flag to control the generation of SBOM')
	cli.sa(longOpt:'sbomAuthor', args:1, argName:'sbomAuthor', 'Author of the SBOM, in form "Name <email>"')

	cli.h(longOpt:'help', 'Prints this message')
	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
		cli.usage()
		rc = 1
	}

	def props = new Properties()

	// read properties file
	if (opts.properties) {
		def propertiesFile = new File(opts.properties)
		if (propertiesFile.exists()) {
			props.packagingPropertiesFile = opts.properties
			propertiesFile.withInputStream { props.load(it) }
		}
	} else { // read default sample properties file shipped with the script
		def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
		def defaultPackagePropFile = new File("$scriptDir/packageBuildOutputs.properties")
		if (defaultPackagePropFile.exists()) {
			props.packagingPropertiesFile = "$scriptDir/packageBuildOutputs.properties"
			defaultPackagePropFile.withInputStream { props.load(it) }
		}
	}

	// set command line arguments
	if (opts.w) props.workDir = opts.w
	if (opts.d) props.deployTypeFilter = opts.d
	if (opts.t) props.tarFileName = opts.t
	if (opts.il) props.includeLogs = opts.il
	if (opts.a) props.application = opts.a
	if (opts.b) props.branch = opts.b

	props.generateWaziDeployAppManifest = (opts.wd) ? 'true' : false
	
	props.addExtension = (opts.ae) ? 'true' : 'false'
	props.verbose = (opts.verb) ? 'true' : 'false'

	// default log encoding if not specified via config passed in via --properties
	if (!props.fileEncoding) props.fileEncoding = "IBM-1047"

	// Optional Artifact repository info to deploy package

	if (opts.v) props.versionName = opts.v
	props.publish = (opts.p) ? 'true' : 'false'

	// read of artifact repository file
	if (opts.aprop) {
		def propertyFile = new File(opts.aprop)
		if (propertyFile.exists()){
			propertyFile.withInputStream { props.load(it) }
		}
	}

	// read the artifact repo cli options, which take precedence over
	// the properties file

	if (opts.aU) props.'artifactRepository.user' = opts.aU
	if (opts.aP) props.'artifactRepository.password' = opts.aP
	if (opts.au) props.'artifactRepository.url' = opts.au
	if (opts.ar) props.'artifactRepository.repo' = opts.ar
	if (opts.ad) props.'artifactRepository.directory' = opts.ad
	if (opts.ah) props.'artifactRepository.httpClientVersion' = opts.ah

	//add any build reports from the file first, then add any from a CLI after.
	//if no file or CLI, go to default build report
	def buildReports = []
	if (opts.boFile) {
		new File (opts.boFile).eachLine { line ->
			buildReports.add(line)
		}

		if (opts.t == false) {
			println("*! [ERROR] Missing required property 'tarFilename'. 'tarFilename' is only optional when no build report order is specified.")
			rc = 2
		}

	}
	if (opts.bO) {
		opts.bO.split(',').each{
			buildReports.add(it)
		}
		if (opts.t == false) {
			println("*! [ERROR] Missing required property 'tarFilename'. 'tarFilename' is only optional when no build report order is specified.")
			rc = 2
		}
	} else if (buildReports.isEmpty()){
		buildReports = [
			props.workDir + "/BuildReport.json"
		]
	}
	props.buildReportOrder = buildReports

	props.generateSBOM = (opts.sbom) ? 'true' : 'false'
	if (opts.sbomAuthor) {
		props.sbomAuthor = opts.sbomAuthor
	}    

	if (!props.workDir) {
		println("*! [ERROR] Missing Build Working Directory ('-w') property.")
		rc = 2
	}

	if (!props.copyModeMap) {
		println("*! [ERROR] Missing copyModeMap property in the configuration file.")
		rc = 2
	}

	// validate SBOM options
	if (props.generateSBOM && props.generateSBOM.toBoolean() && !props.application) {
		println("*! [ERROR] Missing Application ('-a') property required when generating SBOM.")
		rc = 2
	}
	
	// validate publishing options
	if (props.publish && props.publish.toBoolean()){
		if (!props.'artifactRepository.url') {
			println("*! [ERROR] Missing Artifact Repository URL property. It is required when publishing the package via ArtifactRepositoryHelpers.")
			rc = 2
		}
		if (!props.'artifactRepository.repo') {
			println("*! [ERROR] Missing Artifact Repository Name property. It is required when publishing the package via ArtifactRepositoryHelpers.")
			rc = 2
		}
		if (!props.'artifactRepository.user') {
			println("*! [ERROR] Missing Artifact Repository Username property required when publishing package.")
			rc = 2
		}
		if (!props.'artifactRepository.password') {
			println("*! [ERROR] Missing Artifact Repository Password property required when publishing package.")
			rc = 2
		}
		if (!props.'artifactRepository.directory') {
			println("*! [ERROR] Missing Artifact Repository Directory property required when publishing package.")
			rc = 2
		}
	}

	// assess required options to generate Wazi Deploy application manifest
	if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
		if (!props.branch) {
			println("*! [ERROR] Missing branch ('-b') property required when generating Wazi Deploy Application Manifest file.")
			rc = 2
		}
		if (!props.addExtension || !props.addExtension.toBoolean()) {
			println("*! [ERROR] Missing AddExtension ('-ae') property required when generating Wazi Deploy Application Manifest file.")
			rc = 2
		}
	}
	return props
}

/*
 * relativizePath - converts an absolute path to a relative path from the workspace directory
 */
def relativizePath(String path) {
	if (!path.startsWith('/'))
		return path
	String relPath = new File(props.workDir).toURI().relativize(new File(path.trim()).toURI()).getPath()
	// Directories have '/' added to the end.  Lets remove it.
	if (relPath.endsWith('/'))
		relPath = relPath.take(relPath.length()-1)
	return relPath
}

/*
 * parseCopyModeMap - Parses the CopyModeMap provided in the PackageBuildOutputs.properties file, without using 'evaluate()'
 */
def parseCopyModeMap(String copyModeMapString) {
	HashMap<String, String> copyModeMap = new HashMap<String, String>()
	def copyModes = copyModeMapString.replaceAll("[\\[\\]\"]", "").trim().split(",")
	copyModes.each() { copyMode ->
		def copyModeParts = copyMode.split(":")
		copyModeMap.put(copyModeParts[0].trim(), copyModeParts[1].trim())
	}
	return copyModeMap
}


/*
 * The DeployableArtifact class represent an artifact that can be deployed
 * It defines a file (typically the member name of a dataset (the container) or a file in a zFS directory)
 * and a deployType. Instances of this class are used in the main Map object to represent unique artifacts.
 */
class DeployableArtifact {
	private final String file;
	private final String deployType;
	private final String artifactType;

	DeployableArtifact(String file, String deployType, String artifactType) {
		this.file = file;
		this.deployType = deployType;
		this.artifactType = artifactType;
	}

	@Override
	public int hashCode() {
		String concatenation = file + "." + deployType + "." + artifactType;
		return concatenation.hashCode();
	}

	// Compares to DeployableArtifacts object and check if there are equal or not
	public boolean equals(DeployableArtifact other) {
		return other.file.equals(file) & other.deployType.equals(deployType) & (other.artifactType.equals(artifactType));
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DeployableArtifact) {
			return equals((DeployableArtifact) other)
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return file + "." + deployType;
	}
}

def retrieveBuildResultProperty(PropertiesRecord buildResultPropertiesRecord, String propertyName) {

	if (buildResultPropertiesRecord!=null) {
		buildResultProperties = buildResultPropertiesRecord.getProperties()

		def property = buildResultProperties.find {
			it.key.contains(propertyName)
		}

		if (property) {
			return property.getValue()
		} else {
			return null
		}
	}
}