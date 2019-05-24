@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import java.nio.file.FileSystems
import java.nio.file.Path
import java.nio.file.PathMatcher
import groovy.json.JsonSlurper
import groovy.transform.*

// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def gitUtils= loadScript(new File("GitUtilities.groovy"))
@Field def buildUtils= loadScript(new File("BuildUtilities.groovy"))
@Field String hashPrefix = ':githash:'


def createImpactBuildList(RepositoryClient repositoryClient) {
	// local variables
	Set<String> changedFiles = new HashSet<String>()
	Set<String> deletedFiles = new HashSet<String>()
	
	// get the last build result to get the baseline hashes
	def lastBuildResult = repositoryClient.getLastBuildResult(props.applicationBuildGroup, BuildResult.COMPLETE, BuildResult.CLEAN)
	
	// calculate changed files
	if (lastBuildResult) {
		(changedFiles, deletedFiles) = calculateChangedFiles(lastBuildResult)
	}
	else if (props.topicBranchBuild) {
		// if this is the first topic branch build get the main branch build result
		if (props.verbose) println "** No previous topic branch successful build result. Retrieving last successful main branch build result."
		String mainBranchBuildGroup = "${props.application}-${props.mainBuildBranch}"
		lastBuildResult = repositoryClient.getLastBuildResult(mainBranchBuildGroup, BuildResult.COMPLETE, BuildResult.CLEAN)
		if (lastBuildResult) {
			(changedFiles, deletedFiles) = calculateChangedFiles(lastBuildResult)
		}
		else {
			println "*! No previous topic branch build result or main branch build result exists. Cannot calculate file changes."
		}
	}
	else {
		// else create a fullBuild list
		println "*! No prior build result located.  Building all programs"
		changedFiles = buildUtils.createFullBuildList()
	}

	
	// scan files and update source collection for impact analysis
	updateCollection(changedFiles, deletedFiles, repositoryClient)


	// create build list using impact analysis
	Set<String> buildSet = new HashSet<String>()
	changedFiles.each { changedFile ->
		// if the changed file has a build script then add to build list
		if (ScriptMappings.getScriptName(changedFile)) {
			buildSet.add(changedFile)
			if (props.verbose) println "** Found build script mapping for $changedFile. Adding to build list"
		}
		
		// perform impact analysis on changed file
		if (props.verbose) println "** Performing impact analysis on changed file $changedFile"
		ImpactResolver impactResolver = createImpactResolver(changedFile, props.impactResolutionRules, repositoryClient)
			
		def impacts = impactResolver.resolve()
		impacts.each { impact ->
			def impactFile = impact.getFile()
			if (props.verbose) println "** Found impacted file $impactFile"
			// only add impacted files that have a build script mapped to it
			if (ScriptMappings.getScriptName(impactFile)) {
				buildSet.add(impactFile)
				if (props.verbose) println "** $impactFile is impacted by changed file $changedFile. Adding to build list."
			}
		 }
	}
	
	return buildSet
}


def calculateChangedFiles(BuildResult lastBuildResult) {
	// local variables
	Map<String,String> currentHashes = new HashMap<String,String>()
	Map<String,String> baselineHashes = new HashMap<String,String>()
	Set<String> changedFiles = new HashSet<String>()
	Set<String> deletedFiles = new HashSet<String>()
	
	// create a list of source directories to search
	List<String> directories = []
	if (props.applicationSrcDirs)
		directories.addAll(props.applicationSrcDirs.split(','))
		
	// get the current Git hash for all build directories
	directories.each { dir ->
		dir = buildUtils.getAbsolutePath(dir)
		if (props.verbose) println "** Getting current hash for directory $dir"
		String hash = null
		if (gitUtils.isGitDir(dir)) {
			hash = gitUtils.getCurrentGitHash(dir)
		}
		String relDir = buildUtils.relativizePath(dir)
		if (props.verbose) println "** Storing $relDir : $hash"
		currentHashes.put(relDir,hash)
	}

	// get the baseline hash for all build directories
	directories.each { dir ->
		dir = buildUtils.getAbsolutePath(dir)
		if (props.verbose) println "** Getting baseline hash for directory $dir"
		String key = "$hashPrefix${buildUtils.relativizePath(dir)}"
		String hash = lastBuildResult.getProperty(key)
		String relDir = buildUtils.relativizePath(dir)
		if (props.verbose) println "** Storing $relDir : $hash"
		baselineHashes.put(relDir,hash)
	}

	// calculate the changed and deleted files by diff'ing the current and baseline hashes
	directories.each { dir ->
		dir = buildUtils.getAbsolutePath(dir)
		if (props.verbose) println "** Calculating changed files for directory $dir"
		def changed = []
		def deleted = []
		String baseline = baselineHashes.get(buildUtils.relativizePath(dir))
		String current = currentHashes.get(buildUtils.relativizePath(dir))
		if (!baseline || !current) {
			if (props.verbose) println "*! Skipping directory $dir because baseline or current hash does not exist.  baseline : $baseline current : $current"
		}
		else if (gitUtils.isGitDir(dir)) {
			if (props.verbose) "** Diffing baseline $baseline -> current $current"
			(changed, deleted) = gitUtils.getChangedFiles(dir, baseline, current )
		}
		else {
			if (props.verbose) println "*! Directory $dir not a local Git repository. Skipping."
		}
		
		if (props.verbose) println "*** Changed files for directory $dir:"
		changed.each { file ->
			file = fixFilePath(file, dir)
			changedFiles << file
			if (props.verbose) println "*** $file"
		}
			
		if (props.verbose) println "*** Deleted files for directory $dir:"
		deleted.each { file ->
			file = fixFilePath(file, dir)
			deletedFiles << file
			if (props.verbose) println "*** $file"
		}
	}

	return [changedFiles,deletedFiles]
}

def createImpactResolver(String changedFile, String rules, RepositoryClient repositoryClient) {
	if (props.verbose) println "*** Creating impact resolver for $changedFile with $rules rules"
	
	// create an impact resolver for the changed file
	ImpactResolver resolver = new ImpactResolver().file(changedFile)
	                                              .collection(props.applicationCollectionName)
	                                              .collection(props.applicationOutputsCollectionName)
												  .repositoryClient(repositoryClient)
    // add resolution rules
    if (rules)
       resolver.setResolutionRules(buildUtils.parseResolutionRules(rules))
	   
	return resolver
}

def updateCollection(changedFiles, deletedFiles, RepositoryClient repositoryClient) {

	if (props.verbose) println "** Updating collection ${props.applicationCollectionName}"
	def scanner = new DependencyScanner()
	List<LogicalFile> logicalFiles = new ArrayList<LogicalFile>()
	List<PathMatcher> excludeMatchers = createExcludePatterns()
	
	verifyCollections(repositoryClient) 
	
	// remove deleted files from collection
	deletedFiles.each { file ->
		// files in a collection are stored as relative paths from a source directory
		if (props.verbose) println "*** Deleting logical file for $file"
		repositoryClient.deleteLogicalFile(props.applicationCollectionName, buildUtils.relativizePath(file))
	}
	
	// scan changed files
	changedFiles.each { file -> 
		
		// make sure file is not an excluded file
		if (!matches(file, excludeMatchers)) {
			// files in a collection are stored as relative paths from a source directory
			if (props.verbose) println "*** Scanning file $file (${props.workspace}/${file})"
			def logicalFile = scanner.scan(file, props.workspace)
			
			if (props.verbose) println "*** Logical file for $file =\n$logicalFile"
			logicalFiles.add(logicalFile)
			
			// save logical files in batches of 500 to avoid running out of heap space
			if (logicalFiles.size() == 500) {
				if (props.verbose)
					println "** Storing ${logicalFiles.size()} logical files in repository collection '$props.applicationCollectionName'"
				repositoryClient.saveLogicalFiles(props.applicationCollectionName, logicalFiles);
				if (props.verbose) println(repositoryClient.getLastStatus())
				logicalFiles.clear()
			}
		}
	}
			
	// save logical files
	if (props.verbose)
			println "** Storing ${logicalFiles.size()} logical files in repository collection '$props.applicationCollectionName'"
	repositoryClient.saveLogicalFiles(props.applicationCollectionName, logicalFiles);
	if (props.verbose) println(repositoryClient.getLastStatus())
}

/*
 * saveStaticLinkDependencies - Scan the load module to determine LINK dependencies. Impact resolver can use
 * these to determine that this file gets rebuilt if a LINK dependency changes.
 */
def saveStaticLinkDependencies(String buildFile, String loadPDS, LogicalFile logicalFile, RepositoryClient repositoryClient) {
	if (repositoryClient) {
		LinkEditScanner scanner = new LinkEditScanner()
		if (props.verbose) println "*** Scanning load module for $buildFile"
		LogicalFile scannerLogicalFile = scanner.scan(buildUtils.relativizePath(buildFile), loadPDS)
		if (props.verbose) println "*** Logical file = \n$scannerLogicalFile"
		
		// overwrite original logicalDependencies with load module dependencies
		logicalFile.setLogicalDependencies(scannerLogicalFile.getLogicalDependencies())
		
		// Store logical file and indirect dependencies to the outputs collection
		repositoryClient.saveLogicalFile("${props.applicationOutputsCollectionName}", logicalFile );
	}
}

/*
 * verifyCollections - verifies that the application collections exists. If not it will
 * create or clone the collections.
 * Uses build properties
 */
def verifyCollections(RepositoryClient repositoryClient) {
	if (!repositoryClient) {
		if (props.verbose) println "** Unable to verify collections. No repository client."
		return
	}
		
	String mainCollectionName = "${props.application}-${props.mainBuildBranch}"
	String mainOutputsCollectionName = "${props.application}-${props.mainBuildBranch}-outputs"
	
	// check source collection
	if (!repositoryClient.collectionExists(props.applicationCollectionName)) {
		if (props.topicBranchBuild) {
			if (repositoryClient.collectionExists(mainCollectionName)) {
				repositoryClient.copyCollection(mainCollectionName, props.applicationCollectionName)
				if (props.verbose) println "** Cloned collection ${props.applicationCollectionName} from $mainCollectionName"
			}
			else {
				repositoryClient.createCollection(props.applicationCollectionName)
				if (props.verbose) println "** Created collection ${props.applicationCollectionName}"
			}
		}
		else {
			repositoryClient.createCollection(props.applicationCollectionName)
			if (props.verbose) println "** Created collection ${props.applicationCollectionName}"
		}
	}
	
	// check outputs collection
	if (!repositoryClient.collectionExists(props.applicationOutputsCollectionName)) {
		if (props.topicBranchBuild) {
			if (repositoryClient.collectionExists(mainOutputsCollectionName)) {
				repositoryClient.copyCollection("${mainOutputsCollectionName}", props.applicationOutputsCollectionName)
				if (props.verbose) println "** Cloned collection ${props.applicationOutputsCollectionName} from $mainOutputsCollectionName"
			}
			else {
				repositoryClient.createCollection(props.applicationOutputsCollectionName)
				if (props.verbose) println "** Created collection ${props.applicationOutputsCollectionName}"
			}
		}
		else {
			repositoryClient.createCollection(props.applicationOutputsCollectionName)
			if (props.verbose) println "** Created collection ${props.applicationOutputsCollectionName}"
		}
	}
	
}

def fixFilePath(String file, String dir) {
	String dirName = new File(dir).getName()
	if (file.startsWith(dirName))
		return file
	else
		return "$dirName/$file" as String
}

def createExcludePatterns() {
	List<PathMatcher> pathMatchers = new ArrayList<PathMatcher>()
	if (props.excludeFileList) {
		props.excludeFileList.split(',').each{ filePattern ->
			if (!filePattern.startsWith('glob:') || !filePattern.startsWith('regex:'))
				filePattern = "glob:$filePattern"
			PathMatcher matcher = FileSystems.getDefault().getPathMatcher(filePattern)
			pathMatchers.add(matcher)
		}
	}
		
	
	return pathMatchers
}


def matches(String file, List<PathMatcher> pathMatchers) {
	pathMatchers.each { matcher ->
		Path path = FileSystems.getDefault().getPath(file);
		if ( matcher.matches(path) )
		{
			return true
		}
	}
	return false
}





