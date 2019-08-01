@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*

@Field int timeout = 60000 // 1 minute

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
	process.consumeProcessOutput(gitResponse, gitError)
	process.waitForOrKill(timeout)
	if (gitError) {
		println("*? Warning executing isGitDir($dir). Git command: $cmd error: $gitError")	
	}
	else if (gitResponse) {
		isGit = gitResponse.toString().trim().toBoolean()
	}
		
	return isGit
}

/*
 * Returns the current Git branch
 *
 * @param  String gitDir  		Local Git repository directory
 * @return String gitBranch     The current Git branch
 */
def getCurrentGitBranch(String gitDir) {
	String cmd = "git -C $gitDir rev-parse --abbrev-ref HEAD"
	StringBuffer gitBranch = new StringBuffer()
	StringBuffer gitError = new StringBuffer()
	
	Process process = cmd.execute()
	process.consumeProcessOutput(gitBranch, gitError)
	process.waitForOrKill(timeout)
	if (gitError) {
		println("*! Error executing Git command: $cmd error: $gitError")
	}
	return gitBranch.toString().trim()
}

/*
 * Returns the current Git hash
 *
 * @param  String gitDir  		Local Git repository directory
 * @return String gitHash       The current Git hash
 */
def getCurrentGitHash(String gitDir) {
	String cmd = "git -C $gitDir rev-parse HEAD"
	StringBuffer gitHash = new StringBuffer()
	StringBuffer gitError = new StringBuffer()
	
	Process process = cmd.execute()
	process.consumeProcessOutput(gitHash, gitError)
	process.waitForOrKill(timeout)
	if (gitError) {
		print("*! Error executing Git command: $cmd error: $gitError")
	}
	return gitHash.toString().trim()
}

/*
 * Returns the lst previous Git commit hash
 * 
 * @param String gitDir       Local Git repository directory
 * @return String gitHash     The previous Git commit hash
 */
def getPreviousGitHash(String gitDir) {
	String cmd = "git -C $gitDir --no-pager log -n 1 --skip=1"
	StringBuffer gitStdout = new StringBuffer()
	StringBuffer gitError = new StringBuffer()
	
	Process process = cmd.execute()
	process.consumeProcessOutput(gitStdout, gitError)
	process.waitForOrKill(timeout)
	if (gitError) {
		print("*! Error executing Git command: $cmd error: $gitError")
	}
	else {
		return gitStdout.toString().minus('commit').trim().split()[0]
	}
}

def getChangedFiles(String gitDir, String baseHash, String currentHash) {
	String cmd = "git -C $gitDir --no-pager diff --name-status $baseHash $currentHash"
	def git_diff = new StringBuffer()
	def git_error = new StringBuffer()
	def changedFiles = []
	def deletedFiles = []
	
	def process = cmd.execute()
	process.consumeProcessOutput(git_diff, git_error)
	process.waitForOrKill(timeout)
	
	// handle command error
	if (git_error.size() > 0) {
		println("*! Error executing Git command: $cmd error: $git_error")
		println ("*! Attempting to parse unstable git command for changed files...")
	}
	
	for (line in git_diff.toString().split("\n")) {
		// process files from git diff
		try {
			action = line.split()[0]
			file = line.split()[1]
			// handle deleted files
			if (action == "D") {
				deletedFiles.add(file)
			}
			// handle changed files
			else {
				changedFiles.add(file)
			}
		}
		catch (Exception e) {
			// no changes or unhandled format
		}
	}
	
	return [changedFiles, deletedFiles]
}



