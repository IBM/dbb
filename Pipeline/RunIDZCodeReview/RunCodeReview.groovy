import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.transform.*
import java.nio.file.PathMatcher
import java.nio.file.FileSystems
import java.nio.file.Path

// RunCodeReview.groovy

/**
 * This script invokes IDz CodeReview application via JCL based on the provided BuildReport.json
 * 
 * usage: RunCodeReview.groovy --workDir <path-to-dbb-buildreport> [options]
 *
 * 	options:
 *  	-w,--workDir <dir>			Absolute path to the DBB build output directory
 *  	-l,--logEncoding			(Optional) Defines the Encoding for output files (JCL spool, reports), default UTF-8
 *  	-props,--properties			(Optional) Absolute path to the codereview.properties file
 *  	-p,--preview				(Optional) Preview JCL, do not submit it
 *  	-h,--help					(Optional) Prints this message
 *  
 * 	requires:
 * 		codeview.properties file - externalizes the JCL jobcard, RuleFile and Mappings.
 * 		If --properties not provided via cli, the script looks for it at the location of the script itself 
 *  
 */

def properties = parseInput(args)
def startTime = new Date()
properties.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** Run Code Review started at $properties.startTime")
println("** Properties at startup:")
properties.each{k,v->
	println "   $k -> $v"
}

// read build report data
println("** Read build report data from $properties.workDir/BuildReport.json")
def jsonOutputFile = new File("${properties.workDir}/BuildReport.json")

if(!jsonOutputFile.exists()){
	println("** Build report data at $properties.workDir/BuildReport.json not found")
	System.exit()
}

def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))

// parse build report to find the build result meta info
def buildResult = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT}[0];
def dependencies = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET};

// parse build report to find the build outputs to be deployed.
println("** Find source code processed in the build report.")

// the following example finds all the source code with the provided file extension
List<PathMatcher> fileFilter = createIncludePatterns(properties.codereview_includedFiles)
def sources= buildReport.getRecords().findAll{
	it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS && matches(it.getSource().getAbsolutePath(), fileFilter)
}

// the following example finds all the source code with the provided file extension
List<PathMatcher> fileIncludeFilter = createIncludePatterns(properties.codereview_includedIncludeFiles)
def includes= buildReport.getRecords().findAll{
	it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS && matches(it.getSource().getAbsolutePath(), fileIncludeFilter)
}

println("** Found source code processed in the build report.")
sources.each {		println(" ${it.getSource()} ,  ${it.getDestination()}")	}
println("** Founds include files processed in the build report to extract SYSLIB.")

// List to collect the Copybook libraries for the Steplib
List steplib = new ArrayList<String>()
includes.each {
	println(" ${it.getSource()} ,  ${it.getDestination()}")
	steplib.add(it.getDestination().take(it.getDestination().indexOf("(")))
}
steplib = steplib.toUnique()

println("** Create JCL Stream for IDZ Code Review")
String jobcard = properties.codereview_jobcard.replace("\\n", "\n")
JCLExec codeRev=createCodeReviewExec(jobcard, properties.codereview_crRulesFile, sources, steplib)

if (properties.preview.toBoolean()){
	println "** Preview only."
}
else {

	// Execute jclExec
	println "** Execute IDZ Code Review Application via JCL."
	codeRev.execute()
	// Get Job information
	println "   Job '${codeRev.getSubmittedJobId()}' ended with maxRC = ${codeRev.maxRC}"

	// Splitting the String into a StringArray using CC as the seperator
	def jobRcStringArray = codeRev.maxRC.split("CC")

	// This evals the number of items in the ARRAY! Dont get confused with the returnCode itself
	if ( jobRcStringArray.length > 1 ){
		// Ok, the string can be splitted because it contains the keyword CC : Splitting by CC the second record contains the actual RC
		rc = jobRcStringArray[1].toInteger()
		// manage processing the RC, up to your logic. You might want to flag the build as failed.
		if (rc <= 2){
			println   "***  Job ${codeRev.submittedJobId} completed with RC=$rc "}
		else{
			println   "***  Job ${codeRev.submittedJobId} failed with RC=$rc "
			System.exit(1)
		}
	}
	else {
		// We don't see the CC, assume an failure
		println   "***  Job ${codeRev.submittedJobId} failed with ${codeRev.maxRC}"
		System.exit(1)
	}

	println "   Saving spool output to ${properties.workDir}"
	def logFile = new File("${properties.workDir}/CodeReviewSpool-${codeRev.getSubmittedJobId()}.txt")
	codeRev.saveOutput(logFile, properties.logEncoding)

	codeRev.getAllDDNames().each({ ddName ->
		if (ddName == 'XML') {
			def file = new File("${properties.workDir}/CodeReview${ddName}.xml")
			codeRev.saveOutput(ddName, file, properties.logEncoding)
		}
		if (ddName == 'JUNIT') {
			def file = new File("${properties.workDir}/CodeReview${ddName}.xml")
			codeRev.saveOutput(ddName, file, properties.logEncoding)
		}
		if (ddName == 'CSV') {
			def file = new File("${properties.workDir}/CodeReview${ddName}.csv")
			codeRev.saveOutput(ddName, file, properties.logEncoding)
		}
	})

}



/*
 * CodeReviewExec - creates a JCLExec command for CodeReview
 * TODO: Externalize SYSLIB
 */
def createCodeReviewExec(String jobcard, String ruleFile, List memberList,List steplib) {
	// Execute JCL from a String value in the script
	def jcl = jobcard
	jcl += """\
//**********************************************************************
//* LIST OF PROGRAMS TO RUN CODEREVIEW                                 *
//**********************************************************************
//*
//AKGCREV EXEC PROC=AKGCR
"""
steplib.eachWithIndex {it, index -> 
	if (index == 0 ) jcl+="//SYSLIB   DD DISP=SHR,DSN=${it} \n"
	else jcl+="//         DD DISP=SHR,DSN=${it} \n"}
jcl +="""//RULES  DD PATH='$ruleFile'
//LIST   DD * 
"""
	def languageMapping = new PropertyMappings("codereview_languageMapping")
	memberList.each{
		hfsFile = CopyToPDS.createMemberName(it.getSource().getName())
		pdsMember = it.getDestination().take(it.getDestination().indexOf("("))
		if (languageMapping.isMapped("COBOL", it.getSource().getAbsolutePath()))
			jcl += "  L=COBOL M=$hfsFile D=$pdsMember \n"
		if (languageMapping.isMapped("PLI", it.getSource().getAbsolutePath()))
			jcl += "  L=PL/1 M=$hfsFile D=$pdsMember \n"
	}

	jcl += """\
//CSV DD SYSOUT=*,RECFM=VB,LRECL=2051
//XML DD SYSOUT=*,RECFM=VB,LRECL=2051
//MSGS DD SYSOUT=*,RECFM=VB,LRECL=2051
//*
//
"""
	println "$jcl"

	// Create jclExec
	def codeRev = new JCLExec().text(jcl)
	return codeRev
}

/**
 * Parse properties file and cmdline arguments
 */
def parseInput(String[] cliArgs){
	def cli = new CliBuilder(usage: "RunCodeReview.groovy --workDir <path-to-dbb-buildreport> [options]", header: "Command Line Options:")
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the DBB directory containing the BuildReport.json')
	cli.props(longOpt:'properties', args:1, '(Optional) Absolute path to the codereview.properties file. If not provided, will look for it at the location of this script.')
	cli.l(longOpt:'logEncoding', args:1, '(Optional) Defines the Encoding for output files (JCL spool, reports), default UTF-8')
	cli.p(longOpt:'preview', '(Optional) Preview JCL for CR, do not submit it')
	cli.h(longOpt:'help', '(Optional) Prints this message.')

	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
		cli.usage()
		System.exit(0)
	}

	// Instance of DBB Build Properties
	def properties = BuildProperties.getInstance()

	// Identify config file
	if (opts.props){ // if property file is supplied via cli
		buildPropFile = new File(opts.props)

	} else { // looking at the default location
		def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
		buildPropFile = new File("$scriptDir/codereview.properties")
	}
	// Import properties from config file
	if (buildPropFile.exists()){
		properties.buildPropFile = buildPropFile.getAbsolutePath()
		properties.load(buildPropFile)
	}else{
		println "!! codereview.properties not found. Existing."
		System.exit(1)
	}

	// Set command line arguments
	if (opts.w) properties.workDir = opts.w
	properties.logEncoding = (opts.l) ? opts.l : "UTF-8"
	properties.preview = (opts.p) ? 'true' : 'false'

	// Validate required properties
	try {
		assert properties.workDir: "Missing commandline property - workDir - build work directory"
		assert properties.codereview_jobcard: "Missing property in properties file - codereview_jobcard - jobcard"
		assert properties.codereview_crRulesFile: "Missing property in properties file - codereview_crRulesFile - code review rules file"
		assert properties.codereview_includedFiles: "Missing property in properties file - codereview_includedFiles - included files filter"
		assert properties.codereview_includedIncludeFiles: "Missing property in properties file - codereview_includedIncludeFiles - reference to syslib datasets"
		assert properties.codereview_languageMapping: "Missing property in properties file - codereview_languageMapping - languge mapping for LIST DD instream"
	} catch (AssertionError e) {
		cli.usage()
		throw e
	}
	return properties
}

/**
 * 
 */
def createIncludePatterns(String includedFiles) {
	List<PathMatcher> pathMatchers = new ArrayList<PathMatcher>()
	if (includedFiles) {
		includedFiles.split(',').each{ filePattern ->
			if (!filePattern.startsWith('glob:') || !filePattern.startsWith('regex:'))
				filePattern = "glob:$filePattern"
			PathMatcher matcher = FileSystems.getDefault().getPathMatcher(filePattern)
			pathMatchers.add(matcher)
		}
	}
	return pathMatchers
}

/**
 * 
 */
def matches(String file, List<PathMatcher> pathMatchers) {
	def result = pathMatchers.any { matcher ->
		Path path = FileSystems.getDefault().getPath(file);
		if ( matcher.matches(path) )
		{
			return true
		}
	}
	return result
}
