@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.metadata.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.yaml.YamlSlurper
import groovy.yaml.YamlBuilder
import groovy.lang.GroovyShell
import groovy.cli.commons.*
import groovy.util.*
import java.nio.file.*


// script properties
@Field Properties props = new Properties()
@Field def artifactRepositoryHelpers // Helpers to download
@Field def applicationDescriptorUtils // Helper to parse Application Descriptor
@Field def packageBuildOutputs // Helpers to download

// Parse arguments from command-line
parseArgs(args)

// Print parms
println("** Script configuration:")
props.sort().each { k,v->
	println "   $k -> $v"
}

// Load and verify helpers
def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent

// Artifact RepositoryHelpers are used to download the package
File artifactRepositoryHelpersScriptFile = new File("${scriptDir}/../../../Pipeline/PackageBuildOutputs/ArtifactRepositoryHelpers.groovy")
if (artifactRepositoryHelpersScriptFile.exists()) {
	artifactRepositoryHelpers = loadScript(artifactRepositoryHelpersScriptFile)
} else {
	println("*! [ERROR] The Artifact Repository Helper script '${props.artifactRepositoryHelpersScript}' doesn't exist. Exiting.")
	System.exit(8)
}

File applicationDescriptorUtilsFile = new File("${scriptDir}/../../../Pipeline/PackageBuildOutputs/utilities/applicationDescriptorUtils.groovy")
if (applicationDescriptorUtilsFile.exists()) {
	applicationDescriptorUtils = loadScript(applicationDescriptorUtilsFile)
} else {
	println("*! [ERROR] The Application Descriptor Helper script '$applicationDescriptorUtilsFile.getName()' was not found. Exiting.")
	System.exit(8)
}

File applicationDescriptorFile = new File(props.applicationDescriptor)
if (!applicationDescriptorFile.exists()) {
	println("*! [ERROR] The Application Descriptor file '${applicationDescriptorFile.getName()}' doesn't exist. Exiting.")
	System.exit(8)
}

// setup import directory
File importFolder = new File("$props.workspace/imports")
if (importFolder.exists()) importFolder.deleteDir()
importFolder.mkdirs()

// setup package cache
tmpPackageDir = (props.enablePackageCache && props.enablePackageCache.toBoolean() && props.packageCacheLocation) ? new File(props.packageCacheLocation) : new File("$props.workspace/imports_download/")
if (!tmpPackageDir.exists()) tmpPackageDir.mkdirs()

def yamlSlurper = new groovy.yaml.YamlSlurper()

// Parse the application descriptor and application configurations based on the defined schema
applicationDescriptor = applicationDescriptorUtils.readApplicationDescriptor(applicationDescriptorFile)


ArrayList<ExternalDependency> externalDependencies = new ArrayList<>()

if (applicationDescriptor.dependencies) {

	// Loop through all dependencies found in AD
	applicationDescriptor.dependencies.each { dependency ->

/*		- name: "retirementCalculator"
		reference: "release"
		version: "1.2.3"
		buildid: "875487"
	  - name: "GenApp"
		reference: "build"
		version: "feature/789-enhance-something"
		buildid: "123456"*/

		// compute tar file name based on build type
		if (dependency.reference.equalsIgnoreCase("release")) {
			assert dependency.version : "Missing dependency version in dependency record"
			assert dependency.buildid : "Missing buildid in dependency record"
			props.put("tarFileName","${dependency.name}-${dependency.version}-${dependency.buildid}.tar")
		} else {
			props.put("tarFileName","${dependency.name}-${dependency.buildid}.tar")	
		}

		props.put("versionName","${dependency.version}") // compute the version name being part of the path
		props.put("artifactRepository.directory", "${dependency.reference}") // compute the main directory to classify builds
		props.put("artifactRepository.repo", "${dependency.name}-repo-local") // Artifact repository name (hard-coded again)

		// The absolute url the package in artifact repo
		artifactUrl = artifactRepositoryHelpers.computeAbsoluteRepositoryUrl(props)
		println artifactUrl
			
		// Construct the path within the Artifact repo
		repositoryName="${dependency.name}-${props.artifactRepositoryNameSuffix}"
		// retrieve path without artifact url
		artifactRelPath = artifactUrl.replaceAll(props.get("artifactRepository.url"),"")
		
		// File in cache / workspace
		tarFile="${tmpPackageDir}/${artifactRelPath}"
		tarFileDir=tarFile.replaceAll(props.tarFileName, "")
		
		println("*** Fetching package '${dependency.name}:${artifactUrl}' ")

		// Generating information for documentation in yaml file of retrieved dependencies
		if (dependency.name != applicationDescriptor.application) {

			ExternalDependency externalDependency = new ExternalDependency()
			// Map information to External Dependency Record
			externalDependency.name = dependency.name // dependency name
			externalDependency.properties = new HashSet()

			// Add url
			Property p_uri = new Property()
			p_uri.key = "uri"
			p_uri.value = artifactUrl
			externalDependency.properties.add(p_uri)
			
			// type
			Property p_version = new Property()
			p_version.key = "version"
			p_version.value = dependency.version
			externalDependency.properties.add(p_version)
			
			// reference
			Property p_reference = new Property()
			p_reference.key = "reference"
			p_reference.value = dependency.reference
			externalDependency.properties.add(p_reference)
		
			// buildid
			Property p_buildid = new Property()
			p_buildid.key = "buildid"
			p_buildid.value = dependency.buildid
			externalDependency.properties.add(p_buildid)

			// Store external dependency information
			externalDependencies.add(externalDependency)
		}

		// download from artifact repo
		
		// foldername in workspace directory
		String includeFolder = "${importFolder}/${dependency.name}"

		if (new File(tarFile).exists()) {
			println("** Package was already found in package cache at '${tarFile}'")
		} else {
			String user = props.artifactRepositoryUser
			String password = props.artifactRepositoryPassword

			if (!(new File("${tarFileDir}").exists())) (new File("${tarFileDir}")).mkdirs()

			println("** Downloading application package '$artifactUrl' from Artifact Repository into ${tarFileDir}.")
			def rc = artifactRepositoryHelpers.download(artifactUrl, tarFile, user, password, true)

			if (rc != 0) {
				println "** Download of application package '$artifactUrl' failed. Process exists. Return code:"$rc
				exitFetchDependencyProcess()
			} else {
				//println "* Download successful."
			}
		}


		File includeFolderFile = new File(includeFolder)
		if (!includeFolderFile.exists()) {
			includeFolderFile.mkdirs()
		}


		println("** Expanding tar file '${tarFile}' to '$includeFolder' ")

		def processCmd = [
			"/bin/sh",
			"-c",
			"tar -C $includeFolder -xvf ${tarFile}"
		]

		def rc = runProcess(processCmd)
		if (rc != 0) {
			println("** [ERROR] Failed to untar '$tarFile' to '$includeFolder' with rc=$rc")
			System.exit(1)
		}

		// Delete temporary download location if cache is not used
		if (!(props.enablePackageCache && props.enablePackageCache.toBoolean())) {
			tmpPackageDir.deleteDir()
		}
	}
}

// Do we actually need this or can this just be obtained from the dependencies?

// Fetch baseline package for internal dependencies such as
// derived copybooks and object decks

//baselineRecord = applicationDescriptor.baselines.find() { baseline ->
//	baseline.branch.equals(props.branch)
//}
//
//if (baselineRecord){
//	version = matchingBaseline.baseline
//	println("** Retrieving baseline package at version '${version}' for '${applicationDescriptor.application}'")
//
//			baselineName=applicationDescriptor.application
//
//			repositoryName="${props.artifactRepositoryNameSuffix}".replaceAll("§application§", baselineName)
//
//			def String artifactUrl
//			def String artifactReference
//			def String artifactRelPath
//
//			if (dependency.version.startsWith("rel-")){
//				artifactRelPath="${repositoryName}/main/release/${baseline.version}"
//				artifactReference="${artifactRelPath}/${baselineName}.tar"
//				artifactUrl="${props.artifactRepositoryUrl}/${artifactReference}"
//			} else {
//				artifactRelPath="${repositoryName}"
//				artifactReference="${artifactRelPath}/${baselineName}.tar"
//				artifactUrl="${props.artifactRepositoryUrl}/${artifactReference}"
//			}
//			println("*** Fetching baseline package '${baselineName}:${baseline.version}' ")
//
//			String tarFile = "${tmpPackageDir}/${artifactReference}"
//			String includeFolder = "${importFolder}/${baselineName}"
//
//			if (new File(tarFile).exists()) {
//				println("** Package was already found in package cache at '${tmpPackageDir}/${artifactRelPath}'")
//			} else {
//				String user = props.artifactRepositoryUser
//				String password = props.artifactRepositoryPassword
//
//				if (!(new File("${tmpPackageDir}/${artifactRelPath}").exists())) (new File("${tmpPackageDir}/${artifactRelPath}")).mkdirs()
//
//				println("** Downloading application package '$artifactUrl' from Artifact Repository into ${tmpPackageDir}/${artifactRelPath}.")
//				def rc = artifactRepositoryHelpers.download(artifactUrl, tarFile as String, user, password, true)
//				println "download complete $rc" // TODO: Error handling in helper
//			}
//
//			// subdir for each dependency
//			File includeFolderFile = new File(includeFolder)
//			if (!includeFolderFile.exists()) {
//				includeFolderFile.mkdirs()
//			}
//
//			// Expand tar file
//			def processCmd = [
//				"/bin/sh",
//				"-c",
//				"tar -C $includeFolder -xvf $tarFile"
//			]
//
//			def rc = runProcess(processCmd)
//			if (rc != 0) {
//				println("** [ERROR] Failed to untar '$tarFile' to '$includeFolder' with rc=$rc")
//				System.exit(1)
//			}
//} else {
//	println("** [INFO] No baseline record found for application '${applicationDescriptor.application}'.")
//}

def yamlBuilder = new YamlBuilder()
// write file
if (props.externalDependenciesFilePath) {

	println("** Write ExternalDependency Record to ${props.externalDependenciesFilePath}")
	yamlBuilder externalDependencies

	externalDependencyFile = new File(props.externalDependenciesFilePath)
	externalDependencyFile.withWriter("IBM-1047") { extDepWriter ->
		extDepWriter.write(yamlBuilder.toString())
	}
}

def exitFetchDependencyProcess(){
	println("** [ERROR] fetchBuildDependencies encountered a problem. Please review log. Exiting")
	System.exit(1)
}

/**
 * Parse CLI config
 */
def parseArgs(String[] args) {

	String usage = 'generateFetchStatements.groovy [options]'

	def cli = new CliBuilder(usage:usage)
	// required sandbox options
	cli.a(longOpt:'applicationDescriptor', args:1, 'Absolute path to the Application Descriptor file')
	cli.w(longOpt:'workspace', args:1, 'Absolute path to the build workspace')
	cli.d(longOpt:'externalDependenciesFilePath', args:1, 'Absolute path to the external dependencies file')
	cli.p(longOpt:'pipelineBackendConfigFilePath', args:1, 'Absolute path to the pipelineBackend.config file')
	cli.b(longOpt:'branch', args:1, 'Current branch of the application')
	cli.c(longOpt:'packageCacheLocation', args:1, 'Location of the Package cache')


	def opts = cli.parse(args)
	if (!opts) {
		System.exit(1)
	}

	if (opts.a) {
		props.applicationDescriptor = opts.a
	} else {
		println("*! [ERROR] Missing path to the Application Descriptor file. Exiting.")
		System.exit(1)
	}

	if (opts.w) {
		props.workspace = opts.w
	} else {
		println("*! [ERROR] Missing path to the Workspace directory. Exiting.")
		System.exit(1)
	}

	if (opts.d) {
		props.externalDependenciesFilePath = opts.d
	} else {
		println("*! [ERROR] Missing path to the External Dependencies file. Exiting.")
		System.exit(1)
	}

	if (opts.b) {
		props.branch = opts.b
	} else {
		println("*! [ERROR] Missing current Branch name. Exiting.")
		System.exit(1)
	}

	if(opts.c){
		props.packageCacheLocation = opts.c
	}

	if (opts.p) {
		def pipelineBackendConfigFile = new File(opts.p)
		if (pipelineBackendConfigFile.exists()) {
			props.pipelineBackendConfigFile = opts.p
			Properties temporaryProperties = new Properties()
			pipelineBackendConfigFile.withInputStream { temporaryProperties.load(it) }
			// artifact repo configuration properties / Map CBS pipelineBackend.config to script properties
			if(temporaryProperties.get("artifactRepositoryUrl")) props.put("artifactRepository.url", temporaryProperties.get("artifactRepositoryUrl"))
			if(temporaryProperties.get("artifactRepositoryUser")) props.put("artifactRepository.user", temporaryProperties.get("artifactRepositoryUser"))
			if(temporaryProperties.get("artifactRepositoryPassword")) props.put("artifactRepository.password", temporaryProperties.get("artifactRepositoryPassword"))
			if(temporaryProperties.get("artifactRepositoryNameSuffix")) props.put("artifactRepositoryNameSuffix", temporaryProperties.get("artifactRepositoryNameSuffix"))
			if(temporaryProperties.get("enablePackageCache")) props.put("enablePackageCache", temporaryProperties.get("enablePackageCache"))
		} else {
			println("*! [ERROR] Configuration file ${opts.p} not found. Exiting.")
			System.exit(1)
		}
	} else {
		println("*! [ERROR] Missing path to the pipelineBackend.config file. Exiting.")
		System.exit(1)
	}
}

/**********************************************************************************
 * run process
 **********************************************************************************/
def runProcess(ArrayList cmd){
	StringBuffer response = new StringBuffer()
	StringBuffer error = new StringBuffer()

	def p = cmd.execute()
	p.waitForProcessOutput(response, error)

	def rc = p.exitValue();
	if (rc != 0) {
		println("*! [ERROR] Execution of command '$cmd' failed with rc=${rc} and message '${error.toString()}'")
	}
	return rc
}

class ExternalDependency {
	String name
	HashSet<Property> properties = new HashSet<>()
}

class Property {
	String key
	String value
}