@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript

import com.ibm.dbb.task.TaskConstants
import com.ibm.dbb.utils.GitUtilities

// Get absolute file path of the current build file
File file = new File(config.getStringVariable(TaskConstants.FILE_PATH))

// Get the full hash of the commit in which the file was most recently updated
String hash = GitUtilities.getFileCurrentGitHash(file, true)
config.setVariable("GIT_HASH", hash)
log.debug("HASH: {}", config.getStringVariable("GIT_HASH"))

// Get the name of the current git branch
String branch = GitUtilities.getCurrentGitBranch(file.getAbsolutePath())
config.setVariable("GIT_BRANCH", branch)
log.debug("BRANCH: {}", config.getStringVariable("GIT_BRANCH"))

return 0
