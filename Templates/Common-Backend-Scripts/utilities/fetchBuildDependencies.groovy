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

File applicationConfigurationFile = new File(props.applicationDependencyConfiguration)
if (!applicationConfigurationFile.exists()) {
	println("*! [ERROR] The Application Configuration file '${props.applicationDependencyConfiguration}' doesn't exist. Exiting.")
	System.exit(1)
}

def yamlSlurper = new groovy.yaml.YamlSlurper()
// Parse the application descriptor and application configurations
applicationDescriptor = yamlSlurper.parse(applicationDescriptorFile)
applicationDependencyConfiguration = yamlSlurper.parse(applicationConfigurationFile)


ArrayList<ExternalDependency> externalDependencies = new ArrayList<>()

// If there are dependencies
if (applicationDescriptor.dependencies) {

	// Loop through all dependencies found in AD
	applicationDescriptor.dependencies.each { dependency ->

		// Look for the dependency declaration in the Application Configuration file
		applicationDependency = applicationDependencyConfiguration.find { it ->
			it.application == dependency.name
		}

		version = dependency.version

		// assess application dependency if found
		if (applicationDependency) {
			ExternalDependency externalDependency = new ExternalDependency()
			// Map information to External Dependency Record
			externalDependency.name = dependency.name // dependency name
			externalDependency.type = dependency.type // dependency type
			externalDependency.properties = new HashSet()

			if (dependency.type.equals("artifactrepository")) {
				// download from artifactory

				if (version.equals("latest")) {
					println("* Retrieving production version of application '${applicationDependency.application}'")
					version = applicationDependency.productionVersion
				}

				repositoryInfo = applicationDependency.repositories.find {it ->
					it.name == dependency.type
				}
				repositoryName="${props.artifactRepositoryNamePattern}".replaceAll("§application§", dependency.name)

				// this replicates the default format of package build outputs

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

				// Generating information for packaging
				Property p_uri = new Property()
				p_uri.key = "uri"
				p_uri.value = artifactReference
				externalDependency.properties.add(p_uri)
				Property p_version = new Property()
				p_version.key = "version"
				p_version.value = dependency.version
				externalDependency.properties.add(p_version)

				importFolder = new File("$props.workspace/imports")
				if (importFolder.exists()) importFolder.deleteDir()
				importFolder.mkdirs()

				cacheFolder = new File("/tmp/.pkg")
				if (!cacheFolder.exists()) cacheFolder.mkdirs()


				String tarFile = "${cacheFolder}/${artifactReference}"
				String includeFolder = "${importFolder}/${applicationDependency.application}"

				if (new File(tarFile).exists()) {
					println("** Package was already found in package cache at '${cacheFolder}/${artifactRelPath}'")
				} else {
					String user = props.artifactRepositoryUser
					String password = props.artifactRepositoryPassword

					if (!(new File("${cacheFolder}/${artifactRelPath}").exists())) (new File("${cacheFolder}/${artifactRelPath}")).mkdirs()

					println("** Downloading application package '$artifactUrl' from Artifact Repository into ${cacheFolder}/${artifactRelPath}.")
					def rc = artifactRepositoryHelpers.download(artifactUrl, tarFile as String, user, password, true)
					println "download complete $rc" // TODO: Error handling in helper
				}


				File includeFolderFile = new File(includeFolder)
				if (!includeFolderFile.exists()) {
					includeFolderFile.mkdirs()
				}


				println("** Expanding tar file '$tarFile' to '$includeFolder' ")

				def processCmd = [
					"sh",
					"-c",
					"tar -C $includeFolder -xvf $tarFile"
				]

				def rc = runProcess(processCmd)
				if (rc != 0) {
					println("** [ERROR] Failed to untar '$tarFile' to '$includeFolder' with rc=$rc")
					System.exit(1)
				}

			} else {
				println("* Dependency Types other than 'artifactrepository' are not yet implemented. Exiting.")
				System.exit(1)
			}

			// Store external dependency information
			externalDependencies.add(externalDependency)

		} else {
			println("*! [ERROR] Application dependency '${dependency.name}' not documented in Application Configurations File '${props.applicationConfigurations}'. Exiting.")
			System.exit(1)
		}
	}
}

// Do we actually need this or can this just be obtained from the dependencies?

//Pulling in the baseline package
matchingBaseline = applicationDescriptor.baselines.find() { baseline ->
	baseline.branch.equals(props.branch)
}
//if (matchingBaseline0) {
if ( 1 == 0 ){
	version = matchingBaseline.baseline
	println("** Retrieving baseline package at version '${version}' for '${applicationDescriptor.application}'")
	matchingDependency = applicationDependencyConfiguration.find { dependency ->
		dependency.application == applicationDescriptor.application
	}
	if (matchingDependency) {
		repositoryInfo = matchingDependency.repositories.find { repository ->
			repository.name.equals("binary")
		}
		if (repositoryInfo) {
			ExternalDependency externalDependency = new ExternalDependency()
			// Map information to External Dependency Record
			externalDependency.name = applicationDescriptor.application
			externalDependency.type = "binary"
			externalDependency.properties = new HashSet()

			// Adding key value pairs
			Property p_uri = new Property()
			p_uri.key = "uri"
			p_uri.value = repositoryInfo.path
			externalDependency.properties.add(p_uri)
			Property p_version = new Property()
			p_version.key = "version"
			p_version.value = version
			externalDependency.properties.add(p_version)
			externalDependencies.add(externalDependency)

			String artifactUrl="${repositoryInfo.path}/${version}/${applicationDescriptor.application}-${version}.tar"
			String tarFile = "${props.workspace}/imports/${applicationDescriptor.application}.tar"
			String includeFolder = "${props.workspace}/imports/${applicationDescriptor.application}"
			String user = props.artifactRepositoryUser
			String password = props.artifactRepositoryPassword
			artifactRepositoryHelpers.download(artifactUrl, tarFile as String, user, password, true)

			File includeFolderFile = new File(includeFolder)
			if (!includeFolderFile.exists()) {
				includeFolderFile.mkdirs()
			}

			def processCmd = [
				"sh",
				"-c",
				"tar -C $includeFolder -xvf $tarFile"
			]

			def rc = runProcess(processCmd)
			if (rc != 0) {
				println("** [ERROR] Failed to untar '$tarFile' to '$includeFolder' with rc=$rc")
				System.exit(1)
			}
		} else {
			println("*! [ERROR] Couldn't find the application's 'binary' repository in the Application Configuration file. Exiting.")
			System.exit(1)
		}
	} else {
		println("*! [ERROR] Couldn't find the application '${applicationDescriptor.application}' in the Application Configuration file. Exiting.")
		System.exit(1)
	}
} else {
	//println("*! [ERROR] Couldn't determine a baseline in the Application Descriptor file for application '${applicationDescriptor.application}'. Exiting.")
	//System.exit(1)
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

	if (opts.p) {
		def pipelineBackendConfigFile = new File(opts.p)
		if (pipelineBackendConfigFile.exists()) {
			props.pipelineBackendConfigFile = opts.p
			Properties temporaryProperties = new Properties()
			pipelineBackendConfigFile.withInputStream { temporaryProperties.load(it) }
			props.put("applicationDependencyConfiguration", temporaryProperties.get("applicationDependencyConfiguration"))
			props.put("artifactRepositoryHelpersScript", temporaryProperties.get("artifactRepositoryHelpersScript"))
			props.put("artifactRepositoryUrl", temporaryProperties.get("artifactRepositoryUrl"))
			props.put("artifactRepositoryUser", temporaryProperties.get("artifactRepositoryUser"))
			props.put("artifactRepositoryPassword", temporaryProperties.get("artifactRepositoryPassword"))
			props.put("artifactRepositoryNamePattern", temporaryProperties.get("artifactRepositoryNamePattern"))
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