import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.html.*
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import java.io.*

def parseArgs(String[] cliArgs) {
	def cli = new CliBuilder(usage: 'build.groovy [options] buildfile')
	cli.header = "buildFile:  Relative path (from sourceDir) of the file to build. " +
				 "If file is *.txt then assumed to be buildlist file containing a list of relative path files to build. " +
				 "Build list file can be absolute or relative (from sourceDir) path.\noptions:"
	cli.footer = "All command line options can be provided in a properties file passed in using the -f, --propFile argument. " +
		     "Use the argument long form name as the property name. Property file properties are used as default property " +
		     "values and can be overridden by command line options"
	cli.s(longOpt:'sourceDir', args:1, argName:'dir', 'Absolute path to source directory')
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the build output directory')
	cli.q(longOpt:'hlq', args:1, argName:'hlq', 'High level qualifier for partition data sets')
	cli.f(longOpt:'propFile', args:1, argName:'file', 'Absolute path to a default property file')
	cli.c(longOpt:'collection', args:1, argName:'name', 'Name of the dependency data collection')
	cli.t(longOpt:'team', args:1, argName:'hlq', 'Team build hlq for user build syslib concatenations')
	cli.r(longOpt:'repo', args:1, argName:'url', 'DBB repository URL')
	cli.i(longOpt:'id', args:1, argName:'id', 'DBB repository id')
	cli.p(longOpt:'pw', args:1, argName:'password', 'DBB password')
	cli.P(longOpt:'pwFile', args:1, argName:'file', 'Absolute or relative (from sourceDir) path to file containing DBB password')
	cli.e(longOpt:'logEncoding', args:1, argName:'encoding', 'Encoding of output logs. Default is EBCDIC')
	cli.u(longOpt:'userBuild', 'Flag indicating running a user build')
	cli.E(longOpt:'errPrefix', args:1, argName:'errorPrefix', 'Unique id used for IDz error message datasets')
	cli.h(longOpt:'help', 'Prints this message')
	cli.C(longOpt:'clean', 'Deletes the dependency collection and build reeult group from the DBB repository then terminates (skips build)')

	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
	 	cli.usage()
		System.exit(0)
	}

    return opts
}

def loadProperties(String[] cliArgs) {
    def opts = parseArgs(cliArgs)
	def properties = BuildProperties.getInstance()

	// check to see if there is a default properties file to load first
	if (opts.f)
		properties.load(new File(opts.f))

	// set command line arguments
	if (opts.s) properties.sourceDir = opts.s
	if (opts.w) properties.workDir = opts.w
	if (opts.q) properties.hlq = opts.q
	if (opts.c) properties.collection = opts.c
	if (opts.t) properties.team = opts.t
	if (opts.r) properties.repo = opts.r
	if (opts.i) properties.id = opts.i
	if (opts.p) properties.pw = opts.p
	if (opts.P) properties.pwFile = opts.P
	if (opts.e) properties.logEncoding = opts.e
	if (opts.E) properties.errPrefix = opts.E
	if (opts.u) properties.userBuild = "true"
	
	// handle --clean option
	if (opts.C)  {
		println("** Clean up option selected")
		def repo = getDefaultRepositoryClient()
		
		println("* Deleting dependency collection ${properties.collection}")
		repo.deleteCollection(properties.collection)

		println("* Deleting build result group ${properties.collection}Build")
		repo.deleteBuildResults("${properties.collection}Build")
		
		System.exit(0)
	}

	// validate required properties
	assert properties.sourceDir : "Missing property sourceDir - 'Absolute path to source directory'"
	assert properties.workDir : "Missing property workDir - 'Absolute path to the build output directory'"
	assert properties.hlq : "Missing property hlq - 'High level qualifier for partition data sets'"
	if (!properties.userBuild) {  // if this is a team build then all repository arguments must be set
	       assert properties.repo : "Missing property repo - 'DBB repository URL'"
	       assert properties.id : "Missing property id - 'DBB repository id'"
	       assert properties.collection : "Missing property collection - 'Name of the dependency data collection'"
	       if (!(properties.pw || properties.pwFile)) {
		       assert properties.pw : "Missing property pw - 'DBB password'"
		       assert properties.pwFile : "Missing property pwFile - 'Absolute path to file containing DBB password'"
	       }
	}

	// Set the buildFile or buildList property
	if (opts.arguments()) {
	       def buildFile = opts.arguments()[0]
	       if (buildFile.endsWith(".txt")) {
		      if (buildFile.startsWith("/"))
				properties.buildListFile = buildFile
		      else
				properties.buildListFile = "$properties.sourceDir/$buildFile".toString()
	       }
	       else {
		      properties.buildFile = buildFile
	       }
	}


	// load datasets.properties containing system specific PDS names used by the Application build
	properties.load(new File("$properties.sourceDir/Build/IDE/build/datasets.properties"))
	// load file.properties containing file specific properties like script mappings and CICS/DB2 content flags
	properties.load(new File("$properties.sourceDir/Build/IDE/build/file.properties"))

	return properties
}

def getBuildList() {
    def properties = BuildProperties.getInstance()
    def files = []

	// check to see if a build file was passed in
	if (properties.buildFile) {
		println("** Building file $properties.buildFile")
		files = [properties.buildFile]
	}
	// else check to see if a build list file was passed in
	else if (properties.buildListFile) {
		println("** Building files listed in $properties.buildListFile")
	    files = new File(properties.buildListFile) as List<String>
	}
	// build the entire Application listed in files.txt
	else {
	    println("** Building files listed in $properties.sourceDir/Build/IDE/files.txt")
	    files = new File("$properties.sourceDir/Build/IDE/files.txt") as List<String>
	}	
	return files
}

def createDatasets() {
    def properties = BuildProperties.getInstance()
	def srcOptions = "cyl space(1,20) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library) msg(1)"
	def objOptions = "cyl space(1,20) lrecl(80) dsorg(PO) recfm(F,B) blksize(23440) dsntype(library) msg(1) new"
	def optOptions = "cyl space(1,20) lrecl(256) dsorg(PO) recfm(V,B) blksize(32512) dsntype(library) msg(1)"
	def pgmOptions = "cyl space(1,1) lrecl(0) dsorg(PO) recfm(U) blksize(32760) dsntype(library) msg(1)"
	def ispfOptions = "tracks space(1,1) lrecl(80) dsorg(PS) recfm(F,B) msg(1) new"
	def srcDatasets = ["PLILIB", "COBOLLIB", "ASMLIB", "MACRO"]
	def objDatasets = ["PLI", "COBOL", "ASM"]
	def optDatasets = ["OPTIONS"]
	def pgmDatasets = []
	def ispfDatasets = []
	
	srcDatasets.each { dataset ->
		new CreatePDS().dataset("${properties.hlq}.$dataset").options(srcOptions).create()
	}

	objDatasets.each { dataset ->
		new CreatePDS().dataset("${properties.hlq}.$dataset").options(objOptions).create()
	}

	optDatasets.each { dataset ->
		new CreatePDS().dataset("${properties.hlq}.$dataset").options(optOptions).create()
	}
	
	pgmDatasets.each { dataset ->
		new CreatePDS().dataset("${properties.hlq}.$dataset").options(pgmOptions).create()
	}

	ispfDatasets.each { dataset ->
		new CreatePDS().dataset("${properties.hlq}.$dataset").options(ispfOptions).create()
	}

		if (properties.errPrefix) {
	    def xmlOptions = "tracks space(200,40) dsorg(PS) blksize(27998) lrecl(16383) recfm(v,b) new"
    	new CreatePDS().dataset("${properties.hlq}.${properties.errPrefix}.SYSXMLSD.XML").options(xmlOptions).create()
	}

}

def getDefaultRepositoryClient() {
    def properties = BuildProperties.getInstance()
	def repositoryClient = new RepositoryClient().url(properties.repo)
						     .userId(properties.id)
						     .forceSSLTrusted(true)
    if (properties.pw)
		repositoryClient.setPassword(properties.pw)
    else if (properties.pwFile) {
        def pFile = properties.pwFile
   		if (!pFile.startsWith("/"))
    		pFile = "$properties.sourceDir/$properties.pwFile"
		repositoryClient.setPasswordFile(new File(pFile))
	}
	return repositoryClient
}

def initializeBuildArtifacts() {
    BuildReportFactory.createDefaultReport()
    def properties = BuildProperties.getInstance()
    if (!properties.userBuild) {
        def repo = getDefaultRepositoryClient()
        properties.buildGroup = "${properties.collection}Build" as String
        properties.buildLabel = "build.${properties.startTime}" as String
        def buildResult = repo.createBuildResult(properties.buildGroup, properties.buildLabel)
        buildResult.setState(buildResult.PROCESSING)
        buildResult.save()
        println("** Build result created at ${buildResult.getUrl()}")
    }
}

def getBuildResult() {
    def properties = BuildProperties.getInstance()
    def buildResult = null
    if (!properties.userBuild) {
        def repo = getDefaultRepositoryClient()
        buildResult = repo.getBuildResult(properties.buildGroup, properties.buildLabel)
    }
    return buildResult
}

def generateBuildReport() {
    def properties = BuildProperties.getInstance()
    def jsonOutputFile = new File("${properties.workDir}/BuildReport.json")
    def htmlOutputFile = new File("${properties.workDir}/BuildReport.html")

	// create build report data file
	def buildReportEncoding = "UTF-8"
	def buildReport = BuildReportFactory.getBuildReport()
	buildReport.save(jsonOutputFile, buildReportEncoding)

	// create build report html file
	def htmlTemplate = null  // Use default HTML template.
	def css = null       // Use default theme.
	def renderScript = null  // Use default rendering.
	def transformer = HtmlTransformer.getInstance()
	transformer.transform(jsonOutputFile, htmlTemplate, css, renderScript, htmlOutputFile, buildReportEncoding)
	
	return [ jsonOutputFile, htmlOutputFile ]
}

def getCOBDependencyResolver(String file) {
    def properties = BuildProperties.getInstance()
	def path = new DependencyPath().sourceDir(properties.sourceDir).directory("Samples/copybook")
	def rule = new ResolutionRule().library("SYSLIB").path(path)
    def resolver = new DependencyResolver().sourceDir(properties.sourceDir).file(file).rule(rule)
    if (properties.userBuild)
    	resolver.setScanner(new DependencyScanner())
    else {
        path.setCollection(properties.collection)
        resolver.setCollection(properties.collection)
        resolver.setRepositoryClient(getDefaultRepositoryClient())
    }
    return resolver
}

def getASMDependencyResolver(String file) {
	def properties = BuildProperties.getInstance()
	def path = new DependencyPath().sourceDir(properties.sourceDir).directory("Samples/macro")
	def rule = new ResolutionRule().library("SYSLIB").path(path).category("MACRO")
	def resolver = new DependencyResolver().sourceDir(properties.sourceDir).file(file).rule(rule)
	if (properties.userBuild)
		resolver.setScanner(new DependencyScanner())
	else {
		path.setCollection(properties.collection)
		resolver.setCollection(properties.collection)
		resolver.setRepositoryClient(getDefaultRepositoryClient())
	}
	return resolver
}

def getPLIDependencyResolver(String file) {
	def properties = BuildProperties.getInstance()
	def path = new DependencyPath().sourceDir(properties.sourceDir).directory("Samples/pli_copy")
	def rule = new ResolutionRule().library("SYSLIB").path(path)
	def resolver = new DependencyResolver().sourceDir(properties.sourceDir).file(file).rule(rule)
	if (properties.userBuild)
		resolver.setScanner(new DependencyScanner())
	else {
		path.setCollection(properties.collection)
		resolver.setCollection(properties.collection)
		resolver.setRepositoryClient(getDefaultRepositoryClient())
	}
	return resolver
}

def getCDependencyResolver(String file) {
	def properties = BuildProperties.getInstance()
	def path = new DependencyPath().sourceDir(properties.sourceDir).directory("Samples/header")
	def rule = new ResolutionRule().library("SYSLIB").path(path)
	def resolver = new DependencyResolver().sourceDir(properties.sourceDir).file(file).rule(rule)
	if (properties.userBuild)
		resolver.setScanner(new DependencyScanner())
	else {
		path.setCollection(properties.collection)
		resolver.setCollection(properties.collection)
		resolver.setRepositoryClient(getDefaultRepositoryClient())
	}
	return resolver
}

def updateBuildResult(Map args) {
    def properties = BuildProperties.getInstance()
    def error = args.rc > args.maxRC
    def errorMsg = null
    if (error) {
        errorMsg = "*! The return code (${args.rc}) for ${args.file} exceeded the maximum return code allowed (${args.maxRC})"
    	println(errorMsg)
    	properties.error = "true"
    }
    	
   if (!properties.userBuild) {
    	def buildResult = getBuildResult()
    	def member =  CopyToPDS.createMemberName(args.file)
		if (error) {
			buildResult.setStatus(buildResult.ERROR)
			buildResult.addProperty("error", errorMsg)
			if (args.log != null && args.log.exists())
				buildResult.addAttachment("${member}.log", new FileInputStream(args.log))
		}
		buildResult.save()
	}
}

def finalizeBuildResult(Map args) {
	def properties = BuildProperties.getInstance()
	if (!properties.userBuild) {
		def buildResult = getBuildResult()
		buildResult.setBuildReport(new FileInputStream(args.htmlReport))
		buildResult.setBuildReportData(new FileInputStream(args.jsonReport))
		buildResult.setProperty("filesProcessed", String.valueOf(args.filesProcessed))
		buildResult.setState(buildResult.COMPLETE)
		buildResult.save()
	}
}

def buildPrograms(buildOrder, buildList) {
	println("** Invoking build scripts according to build order: ${buildOrder.reverse()}")
	def properties = BuildProperties.getInstance()
	def processCounter = 0
	buildOrder.each { script ->
		// Use the ScriptMappings class to get the files mapped to the build script
		def buildFiles = ScriptMappings.getMappedList(script, buildList)
		def scriptName = "$properties.sourceDir/Build/IDE/build/scripts/${script}.groovy"
		buildFiles.each { file ->
			run(new File(scriptName), [file] as String[])
			processCounter++
		}
	}
	return processCounter	
}




