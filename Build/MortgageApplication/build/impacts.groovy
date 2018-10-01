@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.build.*
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import java.nio.file.*
import groovy.time.*

/**
 * This script identifies which programs are impacted from source code changes since the last
 * successful MortgageApplication build. The result of the script is the generation of a build list text 
 * file in the workDir called buildList.txt i.e. ${workDir}/buildList.txt which contains the programs 
 * that need to be rebuilt. The build list file can be used as the buildFile argument for invoking the 
 * MortgageApplication build.groovy script.
 *
 * If the --buildHash option is omitted or if no previous successful MortgageApplication build exists with a 
 * buildHash property, the contents of MortgageApplication/build/files.txt will be copied to ${workDir}/buildList.txt 
 * resulting in a full build.
 *
 * usage:  impacts.groovy [options]
 *
 *  **NOTE - MortgageApplication/build/build.properties will be loaded (if exists) at script startup.
 *           The options listed below can be used to override the build.properties values. 
 *
 * options:
 *  -b,--buildHash <hash>         Git commit hash for the build
 *  -c,--collection <name>        Name of the dependency data collection
 *  -i,--id <id>                  DBB repository id
 *  -p,--pw <password>            DBB password
 *  -P,--pwFile <file>            Absolute path to file containing DBB password
 *  -r,--repo <url>               DBB repository URL
 *  -s,--sourceDir <dir>          Absolute path to source directory
 *  -w,--workDir <dir>            Absolute path to the build output directory
 *
 */
 
// load the Tools.groovy utility script
def tools = loadScript(new File("Tools.groovy"))
def scriptDir = getScriptDir()

// parse command line arguments and load build properties
def usage = "impact.groovy [options]"
def opts = tools.parseArgs(args, usage)
def properties = tools.loadProperties(opts)
tools.validateRequiredProperties(["sourceDir", "workDir", "dbb.RepositoryClient.url", "dbb.RepositoryClient.userId", "password", "collection"])
	
def startTime = new Date()
properties.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** Impact analysis start at $properties.startTime")

// create workdir (if necessary)
new File(properties.workDir).mkdirs()
   
// if buildHash argument omitted, then just copy MortgageApplication/build/files.txt to buildlist and exit
if (properties.buildHash == null) {
    println("** Git commit build hash option (--buildHash) omitted.  Copying $scriptDir/files.txt to $properties.workDir/buildlist.txt")
    Files.copy(Paths.get("$scriptDir/files.txt"), Paths.get("$properties.workDir/buildList.txt"))
	System.exit(0)
}    

// get the last successful build's buildHash
println("** Searching for last successful build commit hash for build group $properties.collection")
def lastBuildHash = null
def repositoryClient = tools.getDefaultRepositoryClient()
def lastBuildResult = repositoryClient.getLastBuildResult(properties.collection, BuildResult.COMPLETE, BuildResult.CLEAN)
if (lastBuildResult)
    lastBuildHash = lastBuildResult.getProperty("buildHash")                  

// if no lastBuildHash, then just copy MortgageApplication/build/files.txt to buildlist and exit
if (lastBuildHash == null) {
    println("Could not locate last successful build commit hash for build group $properties.collection.  Copying $scriptDir/files.txt to $properties.workDir/buildlist.txt")
    Files.copy(Paths.get("$scriptDir/files.txt"), Paths.get("$properties.workDir/buildList.txt"))
	System.exit(0)
}  
else {
	println("Last successful build commit hash located. label : ${lastBuildResult.getLabel()} , buildHash : $lastBuildHash")
}

// execute git command
def cmd = "git diff --name-only $lastBuildHash $properties.buildHash"
def out = new StringBuffer()
def err = new StringBuffer()

println("** Executing Git command: $cmd")
def process = cmd.execute()
process.consumeProcessOutput(out, err)
process.waitForOrKill(1000)

// handle command error
if (err.size() > 0) {
	println "** Error occurred executing git command: ${err.toString()}"
	System.exit(1)
}
def changedFiles = out.readLines()
println("Number of changed files detected since build ${lastBuildResult.getLabel()} : ${changedFiles.size()}")  
println(out)
      
// if no changed files, created empty build list file and exit
if (changedFiles.size() == 0) {
	println("** No changed files detected since last successful build.  Creating empty file $properties.workDir/buildlist.txt")
	new File("$properties.workDir/buildList.txt").createNewFile()
	System.exit(0)
}

// scan the changed files to make sure dependency data is up to date
println("** Scan the changed file list to collect the latest dependency data")
def scanner = new DependencyScanner()
def logicalFiles = [] as List<LogicalFile>

changedFiles.each { file ->
   	println("Scanning changed file $file")
   	def logicalFile = scanner.scan(file, properties.sourceDir)
   	logicalFiles.add(logicalFile)
}

println("** Store the dependency data in repository collection '$properties.collection'")
// create collection if needed
if (!repositoryClient.collectionExists(properties.collection))
   	repositoryClient.createCollection(properties.collection) 
   	   
repositoryClient.saveLogicalFiles(properties.collection, logicalFiles);
println(repositoryClient.getLastStatus())


// resolve impacted programs/files for changed files  
println("** Creating build list by resolving impacted programs/files for changed files")
def buildList = [] as Set<String>
changedFiles.each { changedFile ->
   // if the changed file has a build script then skip impact analysis and add to build list
   if (ScriptMappings.getScriptName(changedFile)) {
   		buildList.add(changedFile)
   		println("Found build script mapping for $changedFile. Adding to build list.")
   }
   println("Searching for programs impacted by changed file $changedFile")
   def resolver = tools.getDefaultImpactResolver(changedFile)
   def impacts = resolver.resolve()
   impacts.each { impact ->
   	  def impactFile = impact.getFile()
   	  // only add impacted files that have a build script mapped to it
   	  if (ScriptMappings.getScriptName(impactFile)) {
   	     println("$impactFile is impacted by changed file $changedFile. Adding to build list.")
   		 buildList.add(impactFile)
   	  }
   }
}

// Write build list to file
println("** Writing buildlist to $properties.workDir/buildlist.txt")
def buildListFile = new File("$properties.workDir/buildList.txt")
buildList.each { file ->
    buildListFile << (file + "\n")
}

// Print end build message
def endTime = new Date()
def duration = TimeCategory.minus(endTime, startTime)
println("** Impact analysis finished at $endTime")
println("** Total # build files calculated = ${buildList.size()}")
println("** Total analysis time : $duration")
