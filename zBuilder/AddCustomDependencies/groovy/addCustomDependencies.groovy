@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript

import com.ibm.dbb.dependency.LogicalDependency
import com.ibm.dbb.dependency.LogicalFile
import com.ibm.dbb.task.TaskConstants
import com.ibm.dbb.metadata.*
import com.ibm.dbb.metadata.Collection
import com.ibm.dbb.build.*
import java.nio.file.Path
import java.nio.file.PathMatcher

/**
 * Add Custom Dependency
 *
 * For each file in SOURCE_FILE, this task looks for a matching package
 * configuration file named config/<memberName>.pkg somewhere in the
 * repository workspace. When found, it adds a custom logical dependency
 * from the changed file to that config file path.
 *
 * Defaults:
 * - lookup member name is derived from the changed file basename without extension
 * - config directory name is "config"
 * - config file extension is ".pkg"
 *
 * Optional task configuration variables:
 * - customDependencyConfigDirectoryName
 * - customDependencyConfigExtension
 */

Set<String> sourceFiles = context.getSetStringVariable(TaskConstants.SOURCE_LIST) ?: [] as Set<String>
boolean verbose = context.getBooleanVariable(TaskConstants.IS_VERBOSE_MODE)

String configDirectoryName = config.getVariable("dependencyFilePath")
String configExtension = config.getVariable("dependencyfileExtension")
List<String> forFilesFilterPatterns = config.getListVariable("forFilesFilter") ?: ["**/*", "**/.*"]
workspacePath = context.getStringVariable(TaskConstants.WORKSPACE)

if (!configDirectoryName) {
	println ">> ERROR: Missing task configuration 'dependencyFilePath'. Exiting."
	exit 1
}

if (!configExtension) {
	println ">> ERROR: Missing task configuration 'dependencyfileExtension'. Exiting."
	exit 1
}

// validate configuration variables of the task
if (!configExtension.startsWith(".")) {
	configExtension = ".${configExtension}"
}

String appDir = context.getStringVariable("APP_DIR")

if (verbose) {
	println "> Add custom dependency from matching repository config files"
	println "> Application directory: ${appDir}"
	println "> Config directory name: ${configDirectoryName}"
	println "> Config extension: ${configExtension}"
}

if (sourceFiles.isEmpty()) {
	println ">> No changed files found. No custom dependencies added."
	return 0
}

int configMatchesFound = 0

BuildGroup buildGroup = context.getBuildGroup(TaskConstants.BUILD_GROUP)
Collection sourceCollection = buildGroup.getCollection(TaskConstants.SOURCES)

if (sourceCollection == null) {
	if (verbose) {
		println ">> The collection 'sources' was not found in BuildGroup.}"
	}
	return
}

List<LogicalFile> updatedLogicalFiles = new ArrayList<LogicalFile>()

sourceFiles.each {
	
	String sourceFile ->
	if (!matchesForFilesFilter(sourceFile, forFilesFilterPatterns)) {
		if (verbose) {
			println ">> Skipping ${sourceFile} because it does not match forFilesFilter"
		}
		return
	}

	String memberName = deriveMemberName(sourceFile)
	String dependencyTarget = findMatchingConfigPath(appDir, sourceFile, memberName, configDirectoryName, configExtension)

	
	if (dependencyTarget == null) {
		if (verbose) {
			println ">> No matching config file found for ${sourceFile}"
		}
		return
	}

	LogicalFile logicalFile = sourceCollection.getLogicalFile(sourceFile)
	if (logicalFile == null) {
		println ">> WARNING: Unable to resolve LogicalFile for ${sourceFile}. Skipping dependency ${dependencyTarget}"
		return
	}
	String depName = CopyToPDS.createMemberName(dependencyTarget)
	String library = "${configDirectoryName}".toUpperCase()
	String category = configExtension?.replaceFirst(/^\./, '')?.toUpperCase()
	LogicalDependency logicalDependency = new LogicalDependency(depName,library , category)
	logicalFile.addLogicalDependency(logicalDependency)
	updatedLogicalFiles.add(logicalFile)
	configMatchesFound++

	println "> Added custom dependency to file '${sourceFile}'"

}

println "> Updated logical files: ${configMatchesFound}"
sourceCollection.addLogicalFiles(updatedLogicalFiles)

return 0

String deriveMemberName(String sourceFile) {
	String fileName = new File(sourceFile).getName()
	int extensionIndex = fileName.lastIndexOf('.')
	if (extensionIndex > 0) {
		return fileName.substring(0, extensionIndex)
	}
	return fileName
}

String findMatchingConfigPath(String appDir, String sourceFile, String memberName, String configDirectoryName, String configExtension) {
	if (appDir == null) {
		println ">> WARNING: APP_DIR is not set. Unable to check matching config for ${sourceFile}"
		return null
	}

	File configFile = new File("${appDir}/${configDirectoryName}/${memberName}${configExtension}")
	if (!configFile.exists() || !configFile.isFile()) {
		return null
	}

	return toRelativePath(configFile)
}

String toRelativePath(File file) {
	
	if (!file.toURI().getPath().startsWith('/'))
		return file.toURI().getPath()
	
	String relPath = new File(workspacePath as String).toURI().relativize(file.toURI()).getPath()
	// Directories have '/' added to the end.  Lets remove it.
	if (relPath.endsWith('/'))
		relPath = relPath.take(relPath.length()-1)
	return relPath
}


boolean matchesForFilesFilter(String sourceFile, List<String> forFilesFilterPatterns) {
	if (forFilesFilterPatterns == null || forFilesFilterPatterns.isEmpty()) {
		return true
	}

	Path sourceFilePath = Path.of(sourceFile.replace('\\', '/'))
	return forFilesFilterPatterns.any { String pattern ->
		PathMatcher matcher = java.nio.file.FileSystems.getDefault().getPathMatcher("glob:${pattern}")
		return matcher.matches(sourceFilePath)
	}
}
