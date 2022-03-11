import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.transform.*
import groovy.cli.commons.*
import java.nio.file.*;

// PublishPublicInterfaces.groovy

/**
 * This script parses the BuildReport.json 
 *
 * * usage: PublishPublicInterfaces.groovy [options]
 *
 * options:
 *  -w, --workDir <dir>           Absolute path to the DBB build output directory
 *  -tHfs, --targetHfsDirectory	  Path to Shared Git repo
 *  -a, --application			  Application Name 
 *  -v, --verbose                 Enable verbose tracing
 *  -h, --help                    Prints this message
 *
 */

// configuration vars
@Field lockingFile = ".processLock"
@Field deployTypeName = "PublicCopy"
@Field subfolderName = "copybooks"



// start DeployBuildOutputs
@Field properties = parseInput(args)
def startTime = new Date()
properties.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** PublicPublicCopy start at $properties.startTime")
if (properties.verbose) println("** Properties at startup:")
properties.each{k,v->
	if (properties.verbose) println "   $k -> $v"
}

// vars
Path lockFile = Paths.get(properties.targetHfsDirectory);

// read build report data
println("** Read build report data from $properties.workDir/BuildReport.json")
def jsonOutputFile = new File("${properties.workDir}/BuildReport.json")

if(!jsonOutputFile.exists()){
	println("** Build report data at $properties.workDir/BuildReport.json not found")
	System.exit(1)
}

def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))

// parse build report to find the build result meta info
def buildResult = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT}[0];
def dependencies = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET};

// parse build report to find the build outputs to be deployed.
println("** Find and display Public Interfaces in the build report.")

// find all the public interfaces which got copied to the pds
def publicCopies= buildReport.getRecords().findAll{
	it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS &&
			!it.getOutputs().findAll{ o ->
				o.deployType == deployTypeName
			}.isEmpty()
}

// only run logic, if we found Public Interfaces
if (publicCopies.size()!=0){

	setLock(lockFile)

	//    e.q.: /var/jenkins/shared-repo/Shared/<Application>-<subfolder>
	String targetDir = properties.targetHfsDirectory +"/"+properties.application+"-"+subfolderName
	
	if (!isGitDir(properties.targetHfsDirectory)){
		println("*! $targetDir is not a Git repo. Skipping publishing process.")
	} else if(!isGitDetachedHEAD(targetDir)) {
		def gitHash
		publicCopies.each {
			Path originalPath = Paths.get("${it.source}");
			Path targetPath =  Paths.get(targetDir)
			Path targetFile = targetPath.resolve(originalPath.getFileName())

			Files.createDirectories(targetPath)
			println("* Copying   " + originalPath.getFileName() + " to $targetDir")
			Files.copy(originalPath, targetFile, StandardCopyOption.REPLACE_EXISTING);
			// set file tag
			currentTag = processCmd("chtag -p " + originalPath).split("\\s+")[1];
			processCmd("chtag -t -c $currentTag " + targetFile.toString())
			gitHash = getCurrentGitHash(originalPath.getParent().toString())
		}

		//git opertions - git status + add
		processCmd("git -C $targetDir status")
		processCmd("git -C $targetDir add .")
		processCmd("git -C $targetDir status")

		//git commit
		ArrayList nextCmd=new ArrayList<String>()
		nextCmd.add("git")
		nextCmd.add("-C")
		nextCmd.add(targetDir)
		nextCmd.add("commit")
		nextCmd.add("-m")
		nextCmd.add("\"${properties.application}:${gitHash}\"")
		processCmd(nextCmd)

		//git push
		processCmd("git -C $targetDir push")

		// Placeholder: IEBCOPY Files to shared library, to publish files to a shared library

	} else {
		println("*!Git Branch in a detached state. Skipping publishing.")
	}
	//release lock
	releaseLock(lockFile)
}
else {
	println("*! No Public Copybooks found")
}

/*
 * Methods
 */


/*
 * setLock file. Creates a lock file in the shared repository structure to avoid collisions, while working on the Shared repository.
 */
def setLock(Path sharedDir){
	println("** Setting lock file to avoid collisions." )
	def lockAttempts = 0

	while(lockAttempts <  10){
		try{
			Files.createFile(sharedDir.resolve(lockingFile))
			return
		} catch (FileAlreadyExistsException e){
			println("*! $lockingFile exits. Waiting 5 seconds. Attempt : $lockAttempts")
			lockAttempts++
			sleep(5000) // sleep for 5 seconds
		}
	}

	println("*! Could not obtain lock for publishing files. Exiting." )
	System.exit(1)
}

/*
 * releaseLock file.
 */
def releaseLock(Path sharedDir){
	println("* Releasing lock file." )
	Files.deleteIfExists(sharedDir.resolve(lockingFile))
}

/*
 * Returns String containing current hash.
 */
def getCurrentGitHash(String gitDir) {
	String cmd = "git -C $gitDir rev-parse HEAD"
	StringBuffer gitHash = new StringBuffer()
	StringBuffer gitError = new StringBuffer()

	Process process = cmd.execute()
	process.waitForProcessOutput(gitHash, gitError)
	if (gitError) {
		println("*! Error executing Git command: $cmd error: $gitError")
	}
	return gitHash.toString().trim()
}

/*
 * Returns true if this is a detached HEAD
 *
 * @param  String gitDir  		Local Git repository directory
 */
def isGitDetachedHEAD(String gitDir) {
	String cmd = "git -C $gitDir status"
	StringBuffer gitStatus = new StringBuffer()
	StringBuffer gitError = new StringBuffer()

	Process process = cmd.execute()
	process.waitForProcessOutput(gitStatus, gitError)
	if (gitError) {
		println("*! Error executing Git command: $cmd error $gitError")
	}

	return gitStatus.toString().contains("HEAD detached at")
}

/*
 * Tests if directory is in a local git repository
 *
 * @param  String dir  		Directory to test
 * @return boolean
 */
def isGitDir(String dir) {
	String cmd = "git -C $dir rev-parse --is-inside-work-tree"
	StringBuffer gitResponse = new StringBuffer()
	StringBuffer gitError = new StringBuffer()
	boolean isGit = false

	Process process = cmd.execute()
	process.waitForProcessOutput(gitResponse, gitError)
	if (gitError) {
		println("*? Warning executing isGitDir($dir). Git command: $cmd error: $gitError")
	}
	else if (gitResponse) {
		isGit = gitResponse.toString().trim().toBoolean()
	}

	return isGit
}
/*
 * Process any cmd. Exits with exit code:2, if failing.
 */
def processCmd(def cmd){
	println("** Running cmd: $cmd")
	StringBuffer cmdResponse = new StringBuffer()
	StringBuffer cmdError = new StringBuffer()

	Process process = cmd.execute()
	process.waitForProcessOutput(cmdResponse, cmdError)
	if (cmdError) {
		println("*? Error executing cmd. command: $cmd error: $cmdError")
		println("*? The shared repository might be in a inconsistent state. Lock file kept. Please release the conflict.")
		println("*? Exiting with exit code:2 ")
		System.exit(2)
	}
	else if (cmdResponse) {
		if (properties.verbose) println("** Command response: " + cmdResponse.toString())
		return cmdResponse.toString().trim()
	}

}

/*
 * Parses cmd inputs.
 */
def parseInput(String[] cliArgs){
	def cli = new CliBuilder(usage: "RunCodeReview.groovy [options]")
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the DBB directory containing the BuildReport.json')
	cli.tHfs(longOpt:'targetHfsDirectory', args:1, 'Path to Shared Git repo')
	cli.a(longOpt:'application', args:1,'Application Name')
	cli.v(longOpt:'verbose', 'Activate script tracing.')
	cli.h(longOpt:'help', 'Prints this message.')

	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
		cli.usage()
		System.exit(0)
	}

	def properties = new Properties()
	// set command line arguments
	if (opts.w) properties.workDir = opts.w
	if (opts.tHfs) properties.targetHfsDirectory = opts.tHfs
	if (opts.a) properties.application = opts.a
	if (opts.v) properties.verbose = true

	// validate required properties
	try {
		assert properties.workDir: "Missing property build work directory"
		assert properties.targetHfsDirectory: "Missing property path to shared git repo"
		assert properties.application: "Missing property application"
	} catch (AssertionError e) {
		cli.usage()
		throw e
	}
	return properties
}
