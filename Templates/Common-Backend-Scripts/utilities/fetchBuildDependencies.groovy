@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.metadata.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
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
	if ( k == "artifactRepository.password" )
		println "   $k -> xxxxxx "
	else
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
File baselineFolder = new File("$props.workspace/baseline")
if (importFolder.exists()) importFolder.deleteDir()
importFolder.mkdirs()

// setup package cache
tmpPackageDir = (props.enablePackageCache && props.enablePackageCache.toBoolean() && props.packageCacheLocation) ? new File(props.packageCacheLocation) : new File("$props.workspace/imports_download/")
if (!tmpPackageDir.exists()) tmpPackageDir.mkdirs()

// Parse the application descriptor and application configurations based on the defined schema
applicationDescriptor = applicationDescriptorUtils.readApplicationDescriptor(applicationDescriptorFile)


ArrayList<ExternalDependency> externalDependencies = new ArrayList<>()

if (applicationDescriptor.dependencies) {

	// Loop through all dependencies found in AD
	applicationDescriptor.dependencies.each { dependency ->

		// validate dependency record
		assert dependency.type : "Missing dependency type attribute in dependency record"
		assert dependency.reference : "Missing dependency reference attribute in dependency record"
		assert dependency.buildid : "Missing buildid attribute in dependency record"
		assert dependency.name : "Missing name attribute in dependency record"

		// compute tar file name based on build type
		if (dependency.type.equalsIgnoreCase("release")) {
			props.put("tarFileName","${dependency.name}-${dependency.reference}-${dependency.buildid}.tar")
		} else {
			props.put("tarFileName","${dependency.name}-${dependency.buildid}.tar")
		}

		// Construct the path within the Artifact repo
		repositoryName="${dependency.name}-${props.artifactRepositoryNameSuffix}"

		// Prepare for url computation using method in ArtifactRepoHelpers
		props.put("versionName","${dependency.reference}") // compute the version name being part of the path
		props.put("artifactRepository.directory", "${dependency.type}") // compute the main directory to classify builds
		props.put("artifactRepository.repo", "${repositoryName}") // Artifact repository name

		// The absolute url the package in artifact repo
		artifactUrl = artifactRepositoryHelpers.computeAbsoluteRepositoryUrl(props)

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

			// type - either build or release
			Property p_type = new Property()
			p_type.key = "type"
			p_type.value = dependency.type
			externalDependency.properties.add(p_type)

			// reference - the release name or the branch
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

			println("** Downloading archive '$artifactUrl' from Artifact Repository into '${tarFileDir}'.")
			def rc = artifactRepositoryHelpers.download(artifactUrl, tarFile, user, password, true)

			if (rc != 0) {
				println("*! [ERROR] Download of archive '$artifactUrl' failed. Exiting with return code:${rc}.")
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



baselineRecord = applicationDescriptor.baselines.find() { baseline ->
	baseline.branch.equals(props.branch)
}

if (baselineRecord){
	println("*** Fetching baseline package")

	// validate dependency record
	assert baselineRecord.type : "Missing baseline type attribute in baseline record"
	assert baselineRecord.reference : "Missing baseline reference attribute in baseline record"
	assert baselineRecord.buildid : "Missing buildid attribute in baseline record"
	assert applicationDescriptor.application : "Missing application name in Application Descriptor"
	def applicationName = applicationDescriptor.application

	// compute tar file name based on build type
	if (baselineRecord.type.equalsIgnoreCase("release")) {
		props.put("tarFileName","${applicationName}-${baselineRecord.reference}-${baselineRecord.buildid}.tar")
	} else {
		props.put("tarFileName","${applicationName}-${baselineRecord.buildid}.tar")
	}

	// Construct the path within the Artifact repo
	repositoryName="${applicationName}-${props.artifactRepositoryNameSuffix}"

	// Prepare for url computation using method in ArtifactRepoHelpers
	props.put("versionName","${baselineRecord.reference}") // compute the version name being part of the path
	props.put("artifactRepository.directory", "${baselineRecord.type}") // compute the main directory to classify builds
	props.put("artifactRepository.repo", "${repositoryName}") // Artifact repository name

	// The absolute url the package in artifact repo
	artifactUrl = artifactRepositoryHelpers.computeAbsoluteRepositoryUrl(props)

	// retrieve path without artifact url
	artifactRelPath = artifactUrl.replaceAll(props.get("artifactRepository.url"),"")

	// File in cache / workspace
	tarFile="${tmpPackageDir}/${artifactRelPath}"
	tarFileDir=tarFile.replaceAll(props.tarFileName, "")
	if (!tmpPackageDir.exists()) tmpPackageDir.mkdirs() // create tmpDownload

	println("*** Fetching baseline package '${applicationName}:${artifactUrl}' ")

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

	// setup baseline directory
	if (baselineFolder.exists()) baselineFolder.deleteDir()
	baselineFolder.mkdirs()

	println("** Saving tar file '${tarFile}' into '$baselineFolder' ")

	def processCmd = [
		"/bin/sh",
		"-c",
		"cp ${tarFile} ${baselineFolder}/"
	]

	def rc = runProcess(processCmd)
	if (rc != 0) {
		println("** [ERROR] Failed to copy '$tarFile' to '$baselineFolder' with rc=$rc")
		System.exit(1)
	}

	// setup import folder
	String includeFolder = "${importFolder}/${applicationName}"
	File includeFolderFile = new File(includeFolder)
	if (!includeFolderFile.exists()) {
		includeFolderFile.mkdirs()
	}

	println("** Expanding tar file '${tarFile}' to '$includeFolder' ")

	processCmd = [
		"/bin/sh",
		"-c",
		"tar -C $includeFolder -xvf ${tarFile}"
	]

	rc = runProcess(processCmd)
	if (rc != 0) {
		println("** [ERROR] Failed to untar '$tarFile' to '$includeFolder' with rc=$rc")
		System.exit(1)
	}

	// Delete temporary download location if cache is not used
	if (!(props.enablePackageCache && props.enablePackageCache.toBoolean())) {
		tmpPackageDir.deleteDir()
	}
} else {
	println("** [INFO] No baseline record found for application '${applicationDescriptor.application}'.")
}

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