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
@Field def artifactRepositoryHelpers

// Parse arguments from command-line
parseArgs(args)

// Print parms
println("** Script configuration:")
props.each { k,v->
	println "   $k -> $v"
}

File artifactRepositoryHelpersScriptFile = new File(props.artifactRepositoryHelpersScript)
if (artifactRepositoryHelpersScriptFile.exists()) {
	artifactRepositoryHelpers = loadScript(artifactRepositoryHelpersScriptFile)
} else {
	println("*! [ERROR] The Artifact Repository Helper script '${props.artifactRepositoryHelpersScript}' doesn't exist. Exiting.")
	System.exit(1)
}

File applicationDescriptorFile = new File(props.applicationDescriptor)
if (!applicationDescriptorFile.exists()) {
	println("*! [ERROR] The Application Descriptor file '${props.applicationDescriptor}' doesn't exist. Exiting.")
	System.exit(1)
}

// setup import directory
File importFolder = new File("$props.workspace/imports")
if (importFolder.exists()) importFolder.deleteDir()
importFolder.mkdirs()

// setup package cache
tmpPackageDir = (props.enablePackageCache && props.enablePackageCache.toBoolean() && props.packageCacheLocation) ? new File(props.packageCacheLocation) : new File("$props.workspace/imports_download/")
if (!tmpPackageDir.exists()) tmpPackageDir.mkdirs()

def yamlSlurper = new groovy.yaml.YamlSlurper()
// Parse the application descriptor and application configurations
applicationDescriptor = yamlSlurper.parse(applicationDescriptorFile)


ArrayList<ExternalDependency> externalDependencies = new ArrayList<>()

// If there are dependencies
if (applicationDescriptor.dependencies) {

	// Loop through all dependencies found in AD
	applicationDescriptor.dependencies.each { dependency ->

		version = dependency.version

		if (dependency.type.equals("artifactrepository")) {
			// download from artifactory

			// TODO: How do we deal with the latest available?
			//			if (version.equals("latest")) {
			//				println("* Retrieving production version of application '${applicationDependency.application}'")
			//				version = applicationDependency.productionVersion
			//			}

			// Construct the path within the Artifact repo
			repositoryName="${props.artifactRepositoryNamePattern}".replaceAll("§application§", dependency.name)

			def String artifactUrl
			def String artifactReference
			def String artifactRelPath
			if (dependency.version.startsWith("rel-")){
				artifactRelPath="${repositoryName}/main/release/${dependency.version}"
				artifactReference="${artifactRelPath}/${dependency.name}.tar"
				artifactUrl="${props.artifactRepositoryUrl}/${artifactReference}"
			} else {
				artifactRelPath="${repositoryName}"
				artifactReference="${artifactRelPath}/${dependency.name}.tar"
				artifactUrl="${props.artifactRepositoryUrl}/${artifactReference}"
			}
			println("*** Fetching package '${dependency.name}:${dependency.version}' ")

			// Generating information for documentation in yaml file of retrieved dependencies
			if (dependency.name != applicationDescriptor.application) {
				
				ExternalDependency externalDependency = new ExternalDependency()
				// Map information to External Dependency Record
				externalDependency.name = dependency.name // dependency name
				externalDependency.type = dependency.type // dependency type
				externalDependency.properties = new HashSet()
				
				Property p_uri = new Property()
				p_uri.key = "uri"
				p_uri.value = artifactReference
				externalDependency.properties.add(p_uri)
				Property p_version = new Property()
				p_version.key = "version"
				p_version.value = dependency.version
				externalDependency.properties.add(p_version)
				
				// Store external dependency information
				externalDependencies.add(externalDependency)
			}

			String tarFile = "${tmpPackageDir}/${artifactReference}"
			String includeFolder = "${importFolder}/${dependency.name}"

			if (new File(tarFile).exists()) {
				println("** Package was already found in package cache at '${tmpPackageDir}/${artifactRelPath}'")
			} else {
				String user = props.artifactRepositoryUser
				String password = props.artifactRepositoryPassword

				if (!(new File("${tmpPackageDir}/${artifactRelPath}").exists())) (new File("${tmpPackageDir}/${artifactRelPath}")).mkdirs()

				println("** Downloading application package '$artifactUrl' from Artifact Repository into ${tmpPackageDir}/${artifactRelPath}.")
				def rc = artifactRepositoryHelpers.download(artifactUrl, tarFile as String, user, password, true)
				println "download complete $rc" // TODO: Error handling in helper
			}


			File includeFolderFile = new File(includeFolder)
			if (!includeFolderFile.exists()) {
				includeFolderFile.mkdirs()
			}


			println("** Expanding tar file '$tarFile' to '$includeFolder' ")

			def processCmd = [
				"/bin/sh",
				"-c",
				"tar -C $includeFolder -xvf $tarFile"
			]

			def rc = runProcess(processCmd)
			if (rc != 0) {
				println("** [ERROR] Failed to untar '$tarFile' to '$includeFolder' with rc=$rc")
				System.exit(1)
			}

			// Delete temporary download location if cache is not used
			if (!(props.enablePackageCache && props.enablePackageCache.toBoolean())) {tmpPackageDir.deleteDir()}

		} else {
			println("* Dependency Types other than 'artifactrepository' are not yet implemented. Exiting.")
			System.exit(1)
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
//			repositoryName="${props.artifactRepositoryNamePattern}".replaceAll("§application§", baselineName)
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
		props.packageCacheLocation=opts.c
	} 
	
	if (opts.p) {
		def pipelineBackendConfigFile = new File(opts.p)
		if (pipelineBackendConfigFile.exists()) {
			props.pipelineBackendConfigFile = opts.p
			Properties temporaryProperties = new Properties()
			pipelineBackendConfigFile.withInputStream { temporaryProperties.load(it) }
			if(temporaryProperties.get("applicationDependencyConfiguration")) props.put("applicationDependencyConfiguration", temporaryProperties.get("applicationDependencyConfiguration"))
			if(temporaryProperties.get("artifactRepositoryHelpersScript")) props.put("artifactRepositoryHelpersScript", temporaryProperties.get("artifactRepositoryHelpersScript"))
			if(temporaryProperties.get("artifactRepositoryUrl")) props.put("artifactRepositoryUrl", temporaryProperties.get("artifactRepositoryUrl"))
			if(temporaryProperties.get("artifactRepositoryUser")) props.put("artifactRepositoryUser", temporaryProperties.get("artifactRepositoryUser"))
			if(temporaryProperties.get("artifactRepositoryPassword")) props.put("artifactRepositoryPassword", temporaryProperties.get("artifactRepositoryPassword"))
			if(temporaryProperties.get("artifactRepositoryNamePattern")) props.put("artifactRepositoryNamePattern", temporaryProperties.get("artifactRepositoryNamePattern"))
			if(temporaryProperties.get("packageCacheLocation")) props.put("packageCacheLocation", temporaryProperties.get("packageCacheLocation"))
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
	String type
	HashSet<Property> properties = new HashSet<>()
}

class Property {
	String key
	String value
}