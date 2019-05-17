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
 *  -l,--lastBuildHash <hash>     Git commit hash for the last build
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
def opts = parseArgs(args, usage)

// get start time
def startTime = new Date()
println("** Impact analysis start at $startTime")

// create workdir (if necessary)
new File(opts.w).mkdirs()

// return if hashes are same
if (opts.l == opts.b) {
	new File("$opts.w/buildList.txt").delete()
	new File("$opts.w/buildList.txt").createNewFile()
	return
}

// run git diff to find changed files
def cmd = "git -C ${opts.sourceDir} --no-pager diff --name-status ${opts.l} ${opts.b}"
def git_diff = new StringBuffer()
def git_error = new StringBuffer()

println("** Executing Git command: $cmd")
def process = cmd.execute()
process.consumeProcessOutput(git_diff, git_error)
process.waitForOrKill(1000)

// handle command error
if (git_error.size() > 0) {
	println "** Error occurred executing git command: ${git_error.toString()}"
	println "** Attempting to parse unstable git command for changed files..."
}

// build file lists
def changedFiles = []
def deletedFiles = []
println(git_diff.toString().trim())
for (line in git_diff.toString().split("\n")) {
	// process files from git diff
	try {
		action = line.split()[0]
		file = line.split()[1]
		// handle deleted files
		if (action == "D") {
			println("Detected deleted file: ${file}")
			deletedFiles.add(file)
		}
		// handle changed files
		else {
			println("Detected changed file: ${file}")
			changedFiles.add(file)
		}
	}
	catch (Exception e) {
		// no changes or unhandled format
	}
}

println("Number of changed files detected since build ${opts.l} : ${changedFiles.size()}")  
println("Number of deleted files detected since build ${opts.l} : ${deletedFiles.size()}")

// repo client 
def repositoryClient = getDefaultRepositoryClient()
      
// if no changed files, created empty build list file and exit
if (changedFiles.size() == 0) {
	println("** No changed files detected since last successful build.  Creating empty file $opts.w/buildlist.txt")
	new File("$opts.w/buildList.txt").delete()
	new File("$opts.w/buildList.txt").createNewFile()
}

// scan the changed files to make sure dependency data is up to date
println("** Scan the changed file list to collect the latest dependency data")
def scanner = new DependencyScanner()
def logicalFiles = [] as List<LogicalFile>

// add changed files to collection
changedFiles.each { file ->
   	println("Scanning changed file $file")
   	def logicalFile = scanner.scan(file, opts.sourceDir)
   	logicalFiles.add(logicalFile)
}

// create collection if needed
if (!repositoryClient.collectionExists(opts.c))
   	repositoryClient.createCollection(opts.c) 
	
// remove deleted files from collection
deletedFiles.each { file ->
	println("Deleting deleted file $file")
	repositoryClient.deleteLogicalFile(opts.c, file as String)
	
}
   	   
println("** Store the dependency data in repository collection '$opts.c'")
repositoryClient.saveLogicalFiles(opts.c, logicalFiles);
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
println("** Writing buildlist to $opts.w/buildlist.txt")
new File("$opts.w/buildList.txt").delete()
new File("$opts.w/buildList.txt").createNewFile()
def buildListFile = new File("$opts.w/buildList.txt")
buildList.each { file ->
    buildListFile << (file + "\n")
}

// Print end build message
def endTime = new Date()
def duration = TimeCategory.minus(endTime, startTime)
println("** Impact analysis finished at $endTime")
println("** Total # build files calculated = ${buildList.size()}")
println("** Total analysis time : $duration")


def parseArgs(String[] cliArgs, String usage) {
	def cli = new CliBuilder(usage: usage)
	cli.s(longOpt:'sourceDir', args:1, argName:'dir', 'Absolute path to source directory')
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the build output directory')
	cli.b(longOpt:'buildHash', args:1, argName:'hash', 'Git commit hash for the build')
	cli.l(longOpt:'lastBuildHash', args:1, argName:'hash', 'Git commit hash for the last build')
	cli.q(longOpt:'hlq', args:1, argName:'hlq', 'High level qualifier for partition data sets')
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

def getDefaultRepositoryClient() {
	// TODO: Get default repository with arguments instead of properties
	def properties = BuildProperties.getInstance()
	def repositoryClient = new RepositoryClient().forceSSLTrusted(true)
	return repositoryClient
}
