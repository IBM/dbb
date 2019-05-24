@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*
import groovy.json.JsonSlurper



// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field HashSet<String> copiedFileCache = new HashSet<String>()
@Field def gitUtils = loadScript(new File("GitUtilities.groovy"))


/*
 * assertBuildProperties - verify that required build properties for a script exist
 */
def assertBuildProperties(String requiredProps) {
	if (props.verbose) println "required props = $requiredProps"
	if (requiredProps) {
		String[] buildProps = requiredProps.split(',')
	
		buildProps.each { buildProp ->
			buildProp = buildProp.trim()
			assert props."$buildProp" : "*! Missing required build property '$buildProp'"
		}
	}
}

def createFullBuildList() {
	Set<String> buildSet = new HashSet<String>()
	// create the list of build directories
	List<String> srcDirs = []
	if (props.applicationSrcDirs)
		srcDirs.addAll(props.applicationSrcDirs.split(','))
		
	srcDirs.each{ dir ->
		dir = getAbsolutePath(dir)
		buildSet.addAll(getFileSet(dir, true, '**/*.*', props.excludeFileList))
	}
	
	return buildSet
}

/*
 * getFileSet - create a list of files for a directory
 */
def getFileSet(String dir, boolean relativePaths, String includeFileList, String excludeFileList) {
	Set<String> fileSet = new HashSet<String>()

	def files = new FileNameFinder().getFileNames(dir, includeFileList, excludeFileList)
		files.each { file ->
			if (relativePaths)
				fileSet.add(relativizePath(file))
			else
				fileSet.add(file)					
	}
	
	return fileSet
}

/*
 * copySourceFiles - copies both the program being built and the program
 * dependencies from USS directories to data sets
 */
def copySourceFiles(String buildFile, String srcPDS, String dependencyPDS, DependencyResolver dependencyResolver) {
	// only copy the build file once
	if (!copiedFileCache.contains(buildFile)) {
		copiedFileCache.add(buildFile)
		new CopyToPDS().file(new File(getAbsolutePath(buildFile)))
					   .dataset(srcPDS)
					   .member(CopyToPDS.createMemberName(buildFile))
					   .execute()
	}
	
	// resolve the logical dependencies to physical files to copy to data sets
	if (dependencyPDS && dependencyResolver) {
		List<PhysicalDependency> physicalDependencies = dependencyResolver.resolve()
		if (props.verbose) {
			println "*** Resolution rules for $buildFile:"
			dependencyResolver.getResolutionRules().each{ rule ->
				println rule
			}
		}
		if (props.verbose) println "*** Physical dependencies for $buildFile:"
		
		physicalDependencies.each { physicalDependency ->
			if (props.verbose) println physicalDependency
			if (physicalDependency.isResolved()) {
				String physicalDependencyLoc = "${physicalDependency.getSourceDir()}/${physicalDependency.getFile()}"
	
				// only copy the dependency file once per script invocation
				if (!copiedFileCache.contains(physicalDependencyLoc)) {
					copiedFileCache.add(physicalDependencyLoc)
					new CopyToPDS().file(new File(physicalDependencyLoc))
								   .dataset(dependencyPDS)
								   .member(CopyToPDS.createMemberName(physicalDependency.getFile()))
								   .execute()
				}
			}
		}
	}
}

/*
 * sortBuildList - sorts a build list by rank property values
 */
def sortBuildList(List<String> buildList, String rankPropertyName) {
	List<String> sortedList = []
	TreeMap<Integer,List<String>> rankings = new TreeMap<Integer,List<String>>()
	List<String> unranked = new ArrayList<String>()
	
	// sort buildFiles by rank
	buildList.each { buildFile ->
		String rank = props.getFileProperty(rankPropertyName, buildFile)
		if (rank) {
			Integer rankNum = rank.toInteger()
			List<String> ranking = rankings.get(rankNum)
			if (!ranking) {
				ranking = new ArrayList<String>()
				rankings.put(rankNum,ranking)
			}
			ranking << buildFile
		}
		else {
			unranked << buildFile
		}
	}
	
	// loop through rank keys adding sub lists (TreeMap automatically sorts keySet)
	rankings.keySet().each { key ->
		List<String> ranking = rankings.get(key)
		if (ranking)
			sortedList.addAll(ranking)
	}
	
	// finally add unranked buildFiles
	sortedList.addAll(unranked)
	
	return sortedList
}

/*
 * updateBuildResult - used by language scripts to update the build result after a build step
 */
def updateBuildResult(Map args) {
	// args : errorMsg, logs[logName:logFile], client:repoClient
	
	if (args.client) {
		def buildResult = args.client.getBuildResult(props.applicationBuildGroup, props.applicationBuildLabel)
		if (!buildResult) {
			println "*! No build result found for BuildGroup '${props.applicationBuildGroup}' and BuildLabel '${props.applicationBuildLabel}'"
			return
		}
		
		// add error message
		if (args.errorMsg) {
			buildResult.setStatus(buildResult.ERROR)
			buildResult.addProperty("error", args.errorMsg)

		}
		
		// add logs
		if (args.logs) {
			args.logs.each { logName, logFile ->
				if (logFile)
					buildResult.addAttachment(logName, new FileInputStream(logFile))
			}
		}
		
		// save result
		buildResult.save()
	}
}

/*
 * createDependencyResolver - Creates a dependency resolver using resolution rules declared
 * in a build or file property (json format).
 */
def createDependencyResolver(String buildFile, String rules) {
	if (props.verbose) println "*** Creating dependency resolver for $buildFile with $rules rules"
	
	// create a dependency resolver for the build file
	DependencyResolver resolver = new DependencyResolver().file(buildFile)
														  .sourceDir(props.workspace)
														  .scanner(new DependencyScanner())
	// add resolution rules
	if (rules)
		resolver.setResolutionRules(parseResolutionRules(rules))
	
	return resolver
}

def parseResolutionRules(String json) {
	List<ResolutionRule> rules = new ArrayList<ResolutionRule>()
	JsonSlurper slurper = new groovy.json.JsonSlurper()
	List jsonRules = slurper.parseText(json)
	if (jsonRules) {
		jsonRules.each { jsonRule ->
			ResolutionRule resolutionRule = new ResolutionRule()
			resolutionRule.library(jsonRule.library)
			resolutionRule.lname(jsonRule.lname)
			resolutionRule.category(jsonRule.category)
			if (jsonRule.searchPath) {
				jsonRule.searchPath.each { jsonPath ->
					DependencyPath dependencyPath = new DependencyPath()
					dependencyPath.collection(jsonPath.collection)
					dependencyPath.sourceDir(jsonPath.sourceDir)
					dependencyPath.directory(jsonPath.directory)
					resolutionRule.path(dependencyPath)
				}
			}
			rules << resolutionRule
		}
	}
	return rules
}



/*
 * isCICS - tests to see if the program is a CICS program. If the logical file is false, then
 * check to see if there is a file property.
 */
def isCICS(LogicalFile logicalFile) {
	boolean isCICS = logicalFile.isCICS()
	if (!isCICS) {
		String cicsFlag = props.getFileProperty('isCICS', logicalFile.getFile())
		if (cicsFlag)
			isCICS = cicsFlag.toBoolean()
	}
	
	return isCICS
}

/*
 * isSQL - tests to see if the program is an SQL program. If the logical file is false, then
 * check to see if there is a file property.
 */
def isSQL(LogicalFile logicalFile) {
	boolean isSQL = logicalFile.isSQL()
	if (!isSQL) {
		String sqlFlag = props.getFileProperty('isSQL', logicalFile.getFile())
		if (sqlFlag)
			isSQL = sqlFlag.toBoolean()
	}
	
	return isSQL
}

/*
 * isDLI - tests to see if the program is a DL/I program. If the logical file is false, then
 * check to see if there is a file property.
 */
def isDLI(LogicalFile logicalFile) {
	boolean isDLI = logicalFile.isDLI()
	if (!isDLI) {
		String dliFlag = props.getFileProperty('isDLI', logicalFile.getFile())
		if (dliFlag)
			isDLI = dliFlag.toBoolean()
	}
	
	return isDLI
}

/*
 * getAbsolutePath - returns the absolute path of a relative (to workspace) file or directory
 */
def getAbsolutePath(String path) {
	path = path.trim()
	if (path.startsWith('/'))
		return path
		
	String workspace = props.workspace.trim()
	if (!workspace.endsWith('/'))
		workspace = "${workspace}/"
		
	return "${workspace}${path}"
}

/*
 * relativizePath - converts an absolute path to a relative path from the workspace directory
 */
def relativizePath(String path) {
	if (!path)
		return path
	String relPath = new File(props.workspace).toURI().relativize(new File(path.trim()).toURI()).getPath()
	// Directories have '/' added to the end.  Lets remove it.
	if (relPath.endsWith('/'))
		relPath = relPath.take(relPath.length()-1)
	return relPath
}
