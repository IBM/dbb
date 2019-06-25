@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import groovy.time.*

/**
 * This is the main build script for the Mortgage Application.
 *
 * usage: build.groovy [options] buildfile
 * 
 * buildFile:  Relative path (from sourceDir) of the file to build. If file
 * is *.txt then assumed to be buildlist file containing a list of relative
 * path files to build. Build list file can be absolute or relative (from
 * sourceDir) path.
 * 
 * options:
 *  -b,--buildHash <hash>         Git commit hash for the build
 *  -c,--collection <name>        Name of the dependency data collection
 *  -C, --clean                   Deletes the dependency collection and build result group 
 *                                from the DBB repository then terminates (skips build)
 *  -e,--logEncoding <encoding>   Encoding of output logs. Default is EBCDIC
 *  -E,--errPrefix <uniqueId>     Unique id used for IDz error message datasets
 *  -h,--help                     Prints this message
 *  -i,--id <id>                  DBB repository id
 *  -p,--pw <password>            DBB password
 *  -P,--pwFile <file>            Absolute path to file containing DBB password
 *  -q,--hlq <hlq>                High level qualifier for partition data sets
 *  -r,--repo <url>               DBB repository URL
 *  -s,--sourceDir <dir>          Absolute path to source directory
 *  -t,--team <hlq>               Team build hlq for user build syslib concatenations
 *  -u,--userBuild                Flag indicating running a user build
 *  -w,--workDir <dir>            Absolute path to the build output directory
 *  
 * All command line options can be set in the MortgageApplication/build/build.properties file
 * that is loaded at the beginning of the build. Use the argument long form name for the property's
 * name. The properties in build.properties are used as default property values and can be 
 * overridden by command line options.
 */

println("java.version="+System.getProperty("java.runtime.version"))
println("java.home="+System.getProperty("java.home"))
println("user.dir="+System.getProperty("user.dir"))
 
// load the Tools.groovy utility script
def tools = loadScript(new File("Tools.groovy"))

// parse command line arguments and load build properties
def usage = "build.groovy [options] buildfile"
def opts = tools.parseArgs(args, usage)
tools.validateRequiredOpts(opts)
def properties = tools.loadProperties(opts)
if (!properties.userBuild) 
	tools.validateRequiredProperties(["dbb.RepositoryClient.url", "dbb.RepositoryClient.userId", "password", "collection"])
	
println("** Build properties at startup:")
println(properties.list())

def startTime = new Date()
properties.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** Build start at $properties.startTime")

// initialize build artifacts
tools.initializeBuildArtifacts()

// create workdir (if necessary)
new File(properties.workDir).mkdirs()
println("** Build output will be in $properties.workDir")

// create datasets (if necessary)
tools.createDatasets()

// create build list from input build file
def buildList
def incremental = false
if (opts.arguments()) {
	buildList = tools.getBuildList(opts.arguments())
}
// incremental build or full build 
else { 
	
	// get the last successful build's buildHash
	println("** Searching for last successful build commit hash for build group $properties.collection")
	def lastBuildHash = null
	def repositoryClient = tools.getDefaultRepositoryClient()
	def lastBuildResult = repositoryClient.getLastBuildResult(properties.collection, BuildResult.COMPLETE, BuildResult.CLEAN)
	// pull hash from last build result
	if (lastBuildResult)
		lastBuildHash = lastBuildResult.getProperty("buildHash")
	
	// if no last build result, do full build
	if (lastBuildHash == null) {
		println("Could not locate last successful build commit hash for build group $properties.collection.  Building all files.")
		buildList = new File("$scriptDir/files.txt") as List<String>
	}
	else {
		lastBuildHash = lastBuildHash.trim()
		println("Last successful build commit hash located. label : ${lastBuildResult.getLabel()} , buildHash : $lastBuildHash")
		println("** Running incremental build")
		GroovyObject script = loadScript(new File("${properties.sourceDir}/Build/MortgageApplication/build/impacts.groovy"))
		// get arguments needed for impacts.groovy
		impactArguments = tools.getImpactArguments()
		// add last build hash to impact arguments
		impactArguments.add("--lastBuildHash")
		impactArguments.add("${lastBuildHash}")
		// invoke impacts.groovy
		script.invokeMethod("_run", impactArguments as String[])
		// copy output of impacts.groovy to buildlist
		buildList = new File("${properties.workDir}/buildList.txt") as List<String>
		incremental = true
	}
}	

// scan all the files in the process list for dependency data (team build only)
if (!incremental) {
	if (!properties.userBuild && buildList.size() > 0) { 
		// create collection if needed
		def repositoryClient = tools.getDefaultRepositoryClient()
		if (!repositoryClient.collectionExists(properties.collection))
			repositoryClient.createCollection(properties.collection) 
			
		println("** Scan the build list to collect dependency data")
		def scanner = new DependencyScanner()
		def logicalFiles = [] as List<LogicalFile>
		
		buildList.each { file ->
			// ignore whitespace files
			if (file.isAllWhitespace())
				return // only applies to local function
			// scan file
			println("Scanning $file")
			def logicalFile = scanner.scan(file, properties.sourceDir)
			// add file to logical file list
			logicalFiles.add(logicalFile)
			
			if (logicalFiles.size() == 500) {
				println("** Storing ${logicalFiles.size()} logical files in repository collection '$properties.collection'")
				repositoryClient.saveLogicalFiles(properties.collection, logicalFiles);
				println(repositoryClient.getLastStatus())  
				logicalFiles.clear() 		
			}
		}

		println("** Storing remaining ${logicalFiles.size()} logical files in repository collection '$properties.collection'")
		repositoryClient.saveLogicalFiles(properties.collection, logicalFiles);
		println(repositoryClient.getLastStatus())
	}
}

def processCounter = 0
if (buildList.size() == 0)
	println("** No files in build list.  Nothing to build.")
else {
	// build programs by invoking the appropriate build script
	def buildOrder = ["BMSProcessing", "Compile", "CobolCompile", "LinkEdit"]
	// optionally execute IMS MFS builds
	if (properties.BUILD_MFS.toBoolean()) 
		buildOrder << "MFSGENUtility"

	println("** Invoking build scripts according to build order: ${buildOrder[1..-1].join(', ')}")
	buildOrder.each { script ->
    	// Use the ScriptMappings class to get the files mapped to the build script
		def buildFiles = ScriptMappings.getMappedList(script, buildList)
		buildFiles.each { file ->
			runScript(new File("${script}.groovy"), ["file":file])
			processCounter++
		}
	}
}

// generate build report
def (File jsonFile, File htmlFile) = tools.generateBuildReport()

// finalize build result
tools.finalizeBuildResult(jsonReport:jsonFile, htmlReport:htmlFile, filesProcessed:processCounter)

// Print end build message
def endTime = new Date()
def duration = TimeCategory.minus(endTime, startTime)
def state = (properties.error) ? "ERROR" : "CLEAN"
println("** Build ended at $endTime")
println("** Build State : $state")
println("** Total files processed : $processCounter")
println("** Total build time  : $duration")

// if error signal process error for Jenkins to record failed build
if (properties.error)
   System.exit(1)
