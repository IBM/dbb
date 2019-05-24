@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.html.*
import groovy.util.*
import groovy.transform.*
import groovy.time.*
import groovy.xml.*


// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def gitUtils= loadScript(new File("utilities/GitUtilities.groovy"))
@Field def buildUtils= loadScript(new File("utilities/BuildUtilities.groovy"))
@Field def impactUtils= loadScript(new File("utilities/ImpactUtilities.groovy"))
@Field String hashPrefix = ':githash:'
@Field RepositoryClient repositoryClient

// start time message
def startTime = new Date()
props.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("\n** Build start at $props.startTime")

// initialize build
initializeBuildProcess(args)

// create build list
List<String> buildList = createBuildList()

// build programs in the build list
def processCounter = 0
if (buildList.size() == 0)
	println("*! No files in build list.  Nothing to do.")
else {
	if (!props.scanOnly) {
		println("** Invoking build scripts according to build order: ${props.buildOrder}")
		String[] buildOrder = props.buildOrder.split(',')
		buildOrder.each { script ->
			// Use the ScriptMappings class to get the files mapped to the build script
			def buildFiles = ScriptMappings.getMappedList(script, buildList)
			runScript(new File("languages/${script}"), ['buildList':buildFiles])
			processCounter = processCounter + buildFiles.size()
		}
	}
}

// finalize build process
if (processCounter == 0)
	processCounter = buildList.size()
	
finalizeBuildProcess(start:startTime, count:processCounter)

// if error occurred signal process error 
if (props.error)
	System.exit(1)

// end script


//********************************************************************
//* Method definitions
//********************************************************************

def initializeBuildProcess(String[] args) {
	if (props.verbose) println "** Initializing build process . . ."
	
	// build properties initial set
	populateBuildProperties(args)
	
	// verify required build properties
	buildUtils.assertBuildProperties(props.requiredBuildProperties)	
	
	// create a repository client for this script
	if (props."dbb.RepositoryClient.url" && !props.userBuild) {
		repositoryClient = new RepositoryClient().forceSSLTrusted(true)
		println "** Repository client created for ${props.getProperty('dbb.RepositoryClient.url')}"
	}
	
	// handle -r,--reset option
	if (props.reset && props.reset.toBoolean())  {
		println("** Reset option selected")
		if (props."dbb.RepositoryClient.url") {
			repositoryClient = new RepositoryClient().forceSSLTrusted(true)
			
			println("* Deleting collection ${props.applicationCollectionName}")
			repositoryClient.deleteCollection(props.applicationCollectionName)
			
			println("* Deleting collection ${props.applicationOutputsCollectionName}")
			repositoryClient.deleteCollection(props.applicationOutputsCollectionName)
		
			println("* Deleting build result group ${props.applicationBuildGroup}")
			repositoryClient.deleteBuildResults(props.applicationBuildGroup)
		}
		else {
			println("*! No repository client URL provided. Unable to reset!")
		}
		
		System.exit(0)	
	}
	
	// create the work directory (build output)
	new File(props.buildOutDir).mkdirs()
	println("** Build output located at ${props.buildOutDir}")
	
	// verify/create build data sets required by each language script
	if (props.languagePropertyQualifiers) {
		props.languagePropertyQualifiers.trim().split(',').each { lang ->
			if (props."${lang}_srcDatasets")
				createDatasets(props."${lang}_srcDatasets".split(','), props."${lang}_srcOptions")
		
			if (props."${lang}_loadDatasets")
				createDatasets(props."${lang}_loadDatasets".split(','), props."${lang}_loadOptions")
		}
	}
	

	// initialize build report
	BuildReportFactory.createDefaultReport()

	// initialize build result (requires repository connection)
	if (repositoryClient) {
		def buildResult = repositoryClient.createBuildResult(props.applicationBuildGroup, props.applicationBuildLabel)
		buildResult.setState(buildResult.PROCESSING)
		if (props.scanOnly) buildResult.setProperty('scanOnly', 'true')
		if (props.fullBuild) buildResult.setProperty('fullBuild', 'true')
		if (props.impactBuild) buildResult.setProperty('impactBuild', 'true')
		if (props.topicBranchBuild) buildResult.setProperty('topicBranchBuild', 'true')
		if (props.buildFile) buildResult.setProperty('buildFile', XmlUtil.escapeXml(props.buildFile))
		buildResult.save()
		props.buildResultUrl = buildResult.getUrl()
		println("** Build result created for BuildGroup:${props.applicationBuildGroup} BuildLabel:${props.applicationBuildLabel} at ${props.buildResultUrl}")
	}
	
	// verify/create/clone the collections for this build
	impactUtils.verifyCollections(repositoryClient)
}

/*
 * parseArgs - parses build.groovy input options and arguments
 */
def parseArgs(String[] args) {
	String usage = 'build.groovy [options] buildfile'
	String header =  '''buildFile (optional):  Path of the file to build. \
If buildFile is a text file (*.txt) then it is assumed to be a build list file.
options:
	'''
	
	def cli = new CliBuilder(usage:usage,header:header)
	// required sandbox options
	cli.w(longOpt:'workspace', args:1, 'Absolute path to workspace (root) directory containing all required source directories')
	cli.a(longOpt:'application', args:1, required:true, 'Application directory name (relative to workspace)')
	cli.o(longOpt:'outDir', args:1, 'Absolute path to the build output root directory')
	cli.h(longOpt:'hlq', args:1, required:true, 'High level qualifier for partition data sets')
	
	// build options
	cli.p(longOpt:'propFiles', args:1, 'Commas spearated list of additional property files to load. Absolute paths or relative to workspace.')
	cli.l(longOpt:'logEncoding', args:1, 'Encoding of output logs. Default is EBCDIC')
	cli.f(longOpt:'fullBuild', 'Flag indicating to build all programs for application')
	cli.i(longOpt:'impactBuild', 'Flag indicating to build only programs impacted by changed files since last successful build.')
	cli.s(longOpt:'scanOnly', 'Flag indicating to only scan files for application')
	cli.r(longOpt:'reset', 'Deletes the dependency collections and build result group from the DBB repository')
	cli.v(longOpt:'verbose', 'Flag to turn on script trace')
	
	// web application credentials (overrides properties in build.properties)
	cli.url(longOpt:'url', args:1, 'DBB repository URL')
	cli.id(longOpt:'id', args:1, 'DBB repository id')
	cli.pw(longOpt:'pw', args:1,  'DBB repository password')
	cli.pf(longOpt:'pwFile', args:1, 'Absolute or relative (from workspace) path to file containing DBB password')
	
	// IDz/ZOD User build options
	cli.u(longOpt:'userBuild', 'Flag indicating running a user build')
	cli.e(longOpt:'errPrefix', args:1, 'Unique id used for IDz error message datasets')
	cli.srcDir(longOpt:'sourceDir', args:1, 'Absolute path to workspace (root) directory containing all required source directories for user build')
	cli.wrkDir(longOpt:'workDir', args:1, 'Absolute path to the build output root directory for user build')

	// utility options
	cli.help(longOpt:'help', 'Prints this message')
	
	def opts = cli.parse(args)
	
	if(opts.v && args.size() > 1)
		println "** Input args = ${args[1..-1].join(' ')}"
	
	// if help option used, print usage and exit
	if (opts.help) {
		cli.usage()
		System.exit(0)
	}

	return opts
}

/*
 * populateBuildProperties - loads all build property files, creates properties for command line
 * arguments and sets calculated propertied for he build process
 */
def populateBuildProperties(String[] args) {
	
	// parse incoming options and arguments
	def opts = parseArgs(args)
	def zAppBuildDir =  getScriptDir()
	
	// set required command line arguments
	if (opts.w) props.workspace = opts.w
	if (opts.o) props.outDir = opts.o
	if (opts.a) props.application = opts.a
	if (opts.h) props.hlq = opts.h
	
	// need to support IDz user build parameters
	if (opts.srcDir) props.workspace = opts.srcDir
	if (opts.wrkDir) props.outDir = opts.wrkDir
	buildUtils.assertBuildProperties('workspace,outDir')
	
	
	// load build.properties
	def buildConf = "${zAppBuildDir}/build-conf"
	props.load(new File("${buildConf}/build.properties"))
	
	// load additional build property files
	if (props.buildPropFiles) {
		String[] buildPropFiles = props.buildPropFiles.split(',')
		buildPropFiles.each { propFile ->
			if (!propFile.startsWith('/'))
				propFile = "${buildConf}/${propFile}"

			if (opts.v) println "** Loading property file ${propFile}"
			props.load(new File(propFile))
		}
	}
	
	
	// load application.properties
	String appConfRootDir = props.applicationConfRootDir ?: props.workspace
	if (!appConfRootDir.endsWith('/'))
		appConfRootDir = "${appConfRootDir}/"
		
	String appConf = "${appConfRootDir}${props.application}/application-conf"
	if (opts.v) println "** appConf = ${appConf}"
	props.load(new File("${appConf}/application.properties"))
	
	// load additional application property files
	if (props.applicationPropFiles) {
		String[] applicationPropFiles = props.applicationPropFiles.split(',')
		applicationPropFiles.each { propFile ->
			if (!propFile.startsWith('/'))
				propFile = "${appConf}/${propFile}"

			if (opts.v) println "** Loading property file ${propFile}"
			props.load(new File(propFile))
		}
	}

	// load property files from argument list
	if (opts.p) props.propFiles = opts.p
	if (props.propFiles) {
		String[] propFiles = props.propFiles.split(',')
		propFiles.each { propFile ->
			if (!propFile.startsWith('/'))
				propFile = "${props.workspace}/${propFile}"

			if (opts.v) println "** Loading property file ${propFile}"
			props.load(new File(propFile))
		}
	}

	// set optional command line arguments
	if (opts.l) props.logEncoding = opts.l
	if (opts.f) props.fullBuild = 'true'
	if (opts.i) props.impactBuild = 'true'
	if (opts.s) props.scanOnly = 'true'
	if (opts.r) props.reset = 'true'
	if (opts.v) props.verbose = 'true'
	if (opts.url) props.url = opts.url
	if (opts.id) props.id = opts.id
	if (opts.pw) props.pw = opts.pw
	if (opts.pf) props.pf = opts.pf
	
	// set DBB configuration properties
	if (opts.url) props.'dbb.RepositoryClient.url' = opts.url
	if (opts.id) props.'dbb.RepositoryClient.userId' = opts.id
	if (opts.pw) props.'dbb.RepositoryClient.password' = opts.pw
	if (opts.pf) props.'dbb.RepositoryClient.passwordFile' = opts.pf
	
	// set IDz/ZOD user build options
	if (opts.err) props.errPrefix = opts.err
	if (opts.u) props.userBuild = 'true'
	
	// set build file from first non-option argument
	if (opts.arguments()) props.buildFile = opts.arguments()[0].trim()
		
	// set calculated properties
	props.zAppBuildDir = zAppBuildDir
	if (!props.userBuild)
		props.applicationCurrentBranch = gitUtils.getCurrentGitBranch(buildUtils.getAbsolutePath(props.application))
	props.topicBranchBuild = (props.applicationCurrentBranch.equals(props.mainBuildBranch)) ? null : 'true'
	props.applicationBuildGroup = ((props.applicationCurrentBranch) ? "${props.application}-${props.applicationCurrentBranch}" : "${props.application}") as String
	props.applicationBuildLabel = "build.${props.startTime}" as String
        props.applicationCollectionName = ((props.applicationCurrentBranch) ? "${props.application}-${props.applicationCurrentBranch}" : "${props.application}") as String
	props.applicationOutputsCollectionName = "${props.applicationCollectionName}-outputs" as String
	props.buildOutDir = "${props.outDir}/${props.applicationBuildLabel}" as String
	
	if (props.verbose)
		println "** Build properties at start up:\n${props.list()}"

}


def createDatasets(String[] datasets, String options) {
	if (datasets && options) {
		datasets.each { dataset ->
			new CreatePDS().dataset(dataset.trim()).options(options.trim()).create()
			if (props.verbose)
				println "** Creating / verifying build dataset ${dataset}"
		}
	}
}

/*
* createBuildList - creates the list of programs to build. Build list calculated four ways:
*   - full build : Contains all programs in application and external directories. Use script option --fullBuild
*   - impact build : Contains impacted programs from calculated changed files. Use script option --impactBuild
*   - build file : Contains one program. Provide a build file argument.
*   - build text file: Contains a list of programs from a text file. Provide a *.txt build file argument. 
*/
def createBuildList() {
	
	// using a set to create build list to eliminate duplicate files
	Set<String> buildSet = new HashSet<String>()
	String action = (props.scanOnly) ? 'Scanning' : 'Building'

	// check if full build
	if (props.fullBuild) {
		println "** --fullBuild option selected. $action all programs for application ${props.application}"
		buildSet = buildUtils.createFullBuildList()			
	}
	// check if impact build
	else if (props.impactBuild) {
		println "** --impactBuild option selected. $action impacted programs for application ${props.application} "
		if (repositoryClient) {
			buildSet = impactUtils.createImpactBuildList(repositoryClient)
		}
		else {
			println "*! Impact build requires a repository client connection to a DBB web application"
		}
	}
	
	// if build file present add additional files to build list (mandatory build list)
	if (props.buildFile) {
		
		// handle list file	
		if (props.buildFile.endsWith(props.buildListFileExt)) {
			if (!props.buildFile.trim().startsWith('/'))
				props.buildFile = "${props.workspace}/${props.buildFile}" as String
			println "** Adding files listed in ${props.buildFile} to $action build list"

			File jBuildFile = new File(props.buildFile)
			List<String> files = jBuildFile.readLines()
			files.each { file ->
				String relFile = buildUtils.relativizePath(file)
				if (relFile)
					buildSet.add(relFile)
			}
		}
		// else it's a single file to build
		else {
			println "** Adding ${props.buildFile} to $action build list"
			String relFile = buildUtils.relativizePath(props.buildFile)
			if (relFile)
				buildSet.add(relFile)
		}
	}
	
	// now that we are done adding to the build list convert the set to a list
	List<String> buildList = new ArrayList<String>()
	buildList.addAll(buildSet)
	buildSet = null
	
	// scan and update source collection with build list files for non-impact builds
	// since impact build list creation already scanned the incoming changed files
	// we do not need to scan them again
	if (!props.impactBuild && !props.userBuild) {
		impactUtils.updateCollection(buildList, null, repositoryClient)
	}
	
	// write out build list to file (for documentation, not actually used by build scripts)
	String buildListFileLoc = "${props.buildOutDir}/buildList.${props.buildListFileExt}"
	println "** Writing build list file to $buildListFileLoc"
	File buildListFile = new File(buildListFileLoc)	
	String enc = props.logEncoding ?: 'IBM-1047'
	buildListFile.withWriter(enc) { writer ->
		buildList.each { file ->
			if (props.verbose) println file
			writer.write("$file\n")
		}
	}
	
	return buildList
}


def finalizeBuildProcess(Map args) {
	
	// create build report data file
	def jsonOutputFile = new File("${props.buildOutDir}/BuildReport.json")
	println "** Writing build report data to ${jsonOutputFile}"
	def buildReportEncoding = "UTF-8"
	def buildReport = BuildReportFactory.getBuildReport()
	buildReport.save(jsonOutputFile, buildReportEncoding)

	// create build report html file
	def htmlOutputFile = new File("${props.buildOutDir}/BuildReport.html")
	println "** Writing build report to ${htmlOutputFile}"
	def htmlTemplate = null  // Use default HTML template.
	def css = null       // Use default theme.
	def renderScript = null  // Use default rendering.
	def transformer = HtmlTransformer.getInstance()
	transformer.transform(jsonOutputFile, htmlTemplate, css, renderScript, htmlOutputFile, buildReportEncoding)
	
	// update repository artifacts
	if (repositoryClient) {
		if (props.verbose)
			println "** Updating build result BuildGroup:${props.applicationBuildGroup} BuildLabel:${props.applicationBuildLabel}"
		def buildResult = repositoryClient.getBuildResult(props.applicationBuildGroup, props.applicationBuildLabel) 
		buildResult.setBuildReport(new FileInputStream(htmlOutputFile))
		buildResult.setBuildReportData(new FileInputStream(jsonOutputFile))
		buildResult.setProperty("filesProcessed", String.valueOf(args.count))
		buildResult.setState(buildResult.COMPLETE)
		
		// add git hashes for each build directory
		List<String> srcDirs = []
		if (props.applicationSrcDirs)
			srcDirs.addAll(props.applicationSrcDirs.trim().split(','))
			
		srcDirs.each { dir ->
			dir = buildUtils.getAbsolutePath(dir)
			if (props.verbose) println "*** Obtaining hash for directory $dir"
			if (gitUtils.isGitDir(dir)) {
				String hash = gitUtils.getCurrentGitHash(dir)
				String key = "$hashPrefix${buildUtils.relativizePath(dir)}"
				buildResult.setProperty(key, hash)
				if (props.verbose) println "** Setting property $key : $hash"
			}
			else {
				if (props.verbose) println "**! Directory $dir is not a Git repository"
			}
		}
		
		// save build result
		buildResult.save()
	
	}
	
	// print end build message
	def endTime = new Date()
	def duration = TimeCategory.minus(endTime, args.start)
	def state = (props.error) ? "ERROR" : "CLEAN"
	println("** Build ended at $endTime")
	println("** Build State : $state")
	println("** Total files processed : ${args.count}")
	println("** Total build time  : $duration\n")
}



