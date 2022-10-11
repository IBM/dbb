import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import groovy.time.*

/**
 * This is the main build script for the IDz examples.
 *
 * usage: build.groovy [options] buildfile
 *
 * buildFile:  Relative path (from sourceDir) of the file to build. If file
 * is *.txt then assumed to be buildlist file containing a list of relative
 * path files to build. Build list file can be absolute or relative (from
 * sourceDir) path.
 *
 * options:
 *  -c,--collection <name>        Name of the dependency data collection
 *  -C, --clean                   Deletes the dependency collection and build result group
 *                                from the DBB repository then terminates (skips build)
 *  -e,--logEncoding <encoding>   Encoding of output logs. Default is EBCDIC
 *  -E,--errPrefix <uniqueId>     Unique id used for IDz error feedback datasets
 *  -f,--propFile <file>          Absolute path to a default property file
 *  -h,--help                     Prints this message
 *  -i,--id <id>                  DBB repository id
 *  -p,--pw <password>            DBB password
 *  -P,--pwFile <file>            Absolute path to file containing DBB
 *                                password
 *  -q,--hlq <hlq>                High level qualifier for partition data
 *                                sets
 *  -r,--repo <url>               DBB repository URL
 *  -s,--sourceDir <dir>          Absolute path to source directory
 *  -t,--team <hlq>               Team build hlq for user build syslib concatenations
 *  -u,--userBuild                Flag indicating running a user build
 *  -w,--workDir <dir>            Absolute path to the build output directory
 *
 * All command line options can be provided in a properties file passed in
 * using the -f, --propFile <file> argument. Use the argument long form name
 * for the properties name. Property file properties are used as default property
 * values and can be overridden by command line options
 */

// get script directory
def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent

// check to see if there is a ./build.properties to load
def properties = BuildProperties.getInstance()
def buildPropFile = new File("$scriptDir/build.properties")
if (buildPropFile.exists())
   BuildProperties.load(buildPropFile)

// load the Tools.groovy utility script
File sourceFile = new File("$scriptDir/Tools.groovy")
Class groovyClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(sourceFile)
GroovyObject tools = (GroovyObject) groovyClass.newInstance()

// start build
def startTime = new Date()
properties.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** Build start at $properties.startTime")

// load/set build properties from command line arguments
tools.loadProperties(args)
println("** Build properties at startup:")
println(properties.list())

// initialize build artifacts
tools.initializeBuildArtifacts()

// create workdir (if necessary)
new File(properties.workDir).mkdirs()
println("** Build output will be in $properties.workDir")

// create datasets (if necessary)
tools.createDatasets()

// create build list from input build file
def buildList = tools.getBuildList()

// scan all the files in the process list for dependency data (team build only)
if (!properties.userBuild) {
	println("** Scan the build list to collect dependency data")
	def scanner = new DependencyScanner()
	def logicalFiles = [] as List<LogicalFile>
	
	buildList.each { file ->
    	println("Scanning $file")
    	def logicalFile = scanner.scan(file, properties.sourceDir)
    	logicalFiles.add(logicalFile)
	}

	println("** Store the dependency data in repository collection '$properties.collection'")
	// create collection if needed
	def repo = tools.getDefaultRepositoryClient()
	if (!repo.collectionExists(properties.collection))
    	repo.createCollection(properties.collection)
    	
	repo.saveLogicalFiles(properties.collection, logicalFiles);
	println(repo.getLastStatus())
}

// define build script order
def buildOrder = [
	"ASMCompile",
	"CobolCompile",
	"PliCompile"
	]
	
// build programs
fileCount = tools.buildPrograms(buildOrder, buildList)

// generate build report
def (File jsonFile, File htmlFile) = tools.generateBuildReport()

// finalize build result
tools.finalizeBuildResult(jsonReport:jsonFile, htmlReport:htmlFile, filesProcessed:fileCount)


// Print end build message
def endTime = new Date()
def duration = TimeCategory.minus(endTime, startTime)
def state = (properties.error) ? "ERROR" : "CLEAN"
println("** Build finished at $endTime")
println("** Build State : $state")
println("** Total files processed : $fileCount")
println("** Total build time  : $duration")

// if error signal process error for Jenkins to record failed build
if (properties.error)
   System.exit(1)
