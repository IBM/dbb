@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.transform.*
import groovy.cli.commons.*
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

@Field BuildProperties props = BuildProperties.getInstance()
parseInput(args)

def startTime = new Date()
props.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** Run Code Review started at $props.startTime")
println("** Properties at startup:")
props.each{k,v->
	println "   $k -> $v"
}

// read build report data
println("** Read build report data from $props.workDir/BuildReport.json")
def jsonOutputFile = new File("${props.workDir}/BuildReport.json")

if(!jsonOutputFile.exists()){
	println("** Build report data at $props.workDir/BuildReport.json not found")
	System.exit(1)
}

def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))

// parse build report to find the build result meta info
def buildResult = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT}[0];
def dependencies = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET};

// parse build report to find the build outputs to be deployed.
println("** Find source code processed in the build report.")

// the following example finds all the source code with the provided file extension
List<PathMatcher> fileFilter = createIncludePatterns(props.codereview_includedFiles)
def sources= buildReport.getRecords().findAll{
	it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS && matches(it.getSource().getAbsolutePath(), fileFilter)
}

// the following example finds all the source code with the provided file extension
List<PathMatcher> fileIncludeFilter = createIncludePatterns(props.codereview_includedIncludeFiles)
def includes= buildReport.getRecords().findAll{
	it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS && matches(it.getSource().getAbsolutePath(), fileIncludeFilter)
}

println("** Found source code processed in the build report.")
sources.each {		 println(" ${it.getSource()} ,  ${it.getDestination()}")	}
println("** Found include files processed in the build report to extract SYSLIB.")

if (sources.size == 0){
	println("!! No source files found in the build report to process. Skipping Code Review.")
}
else
{

	// List to collect the Copybook libraries for the syslib
	List syslib = new ArrayList<String>()
	includes.each {
		println(" ${it.getSource()} ,  ${it.getDestination()}")
		syslib.add(it.getDestination().take(it.getDestination().indexOf("(")))
	}
	syslib = syslib.toUnique()

	println("** Create JCL Stream for IDZ Code Review")
	String jobcard = props.codereview_jobcard.replace("\\n", "\n")
	JCLExec codeRev=createCodeReviewExec(jobcard, props.codereview_crRulesFile, props.codereview_ccrRulesFile, sources, syslib)

	if (props.preview.toBoolean()){
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

		println "** Saving spool output to ${props.workDir}"
		def logFile = new File("${props.workDir}/CodeReviewSpool-${codeRev.getSubmittedJobId()}.txt")
		codeRev.saveOutput(logFile, props.logEncoding)

		codeRev.getAllDDNames().each({ ddName ->
			if (ddName == 'XML') {
				def ddfile = new File("${props.workDir}/CodeReview${ddName}.xml")
				saveJobOutput(codeRev, ddName, ddfile)
			}
			if (ddName == 'JUNIT') {
				def ddfile = new File("${props.workDir}/CodeReview${ddName}.xml")
				saveJobOutput(codeRev, ddName, ddfile)
			}
			if (ddName == 'CSV') {
				def ddfile = new File("${props.workDir}/CodeReview${ddName}.csv")
				saveJobOutput(codeRev, ddName, ddfile)
			}
		})
	}

}

/*
 * Ensures backward compatibility
 */
def saveJobOutput ( JCLExec codeRev, String ddName, File file) {
	try {
		codeRev.saveOutput(ddName, file, props.logEncoding, true)
	} catch ( Exception ex ) {
		println "*? Warning the output file $file\n*? will have an extra space at the beginning of each line.\n*? Updating DBB to the latest PTF with ASA control characters API for JCLExec is highly recommended."
		codeRev.saveOutput(ddName, file, props.logEncoding)
	}
}

/*
 * CodeReviewExec - creates a JCLExec command for CodeReview
 * TODO: Externalize SYSLIB
 */
def createCodeReviewExec(String jobcard, String ruleFile, String customRuleFile, List memberList, List syslib) {
	// Execute JCL from a String value in the script
	def jcl = jobcard
	jcl += """\
//**********************************************************************
//* LIST OF PROGRAMS TO RUN CODEREVIEW                                 *
//**********************************************************************
//*
//AKGCREV EXEC PROC=AKGCR
"""
	// add default dummy
	jcl+="//SYSLIB   DD DUMMY \n"
	// add identified syslib
	syslib.each { jcl+="//         DD DISP=SHR,DSN=${it} \n"}
	// add syslib libraries from property file
	if (props.codereview_syslib){
		props.codereview_syslib.split(',').each { jcl+="//         DD DISP=SHR,DSN=${it} \n" }
	}
	
	if ( customRuleFile  ) {
		def lines = formatJCLPath("//CUSTRULE  DD PATH='$customRuleFile'")
		lines.each{ jcl += it + "\n" }
	}
	def lines = formatJCLPath("//RULES  DD PATH='$ruleFile'")
	lines.each{ jcl += it + "\n" }

	jcl += "//LIST   DD *\n"
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
	cli.cr(longOpt:'crRulesFile', args:1, '(Optional) Absolute path of the rules file. If not provided, will look for it in the codereview.properties file.')
	cli.ccr(longOpt:'ccrRulesFile', args:1, '(Optional) Absolute path of the custom rules file. If not provided, will look for it in the codereview.properties file.')
	cli.l(longOpt:'logEncoding', args:1, '(Optional) Defines the Encoding for output files (JCL spool, reports), default UTF-8')
	cli.p(longOpt:'preview', '(Optional) Preview JCL for CR, do not submit it')
	cli.h(longOpt:'help', '(Optional) Prints this message.')

	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
		cli.usage()
		System.exit(0)
	}

	// Identify config file
	if (opts.props){ // if property file is supplied via cli
		buildPropFile = new File(opts.props)

	} else { // looking at the default location
		def scriptDir = getScriptDir()
		buildPropFile = new File("$scriptDir/codereview.properties")
	}
	// Import properties from config file
	if (buildPropFile.exists()){
		props.buildPropFile = buildPropFile.getAbsolutePath()
		props.load(buildPropFile)
	}else{
		println "!! codereview.properties not found. Existing."
		System.exit(1)
	}

	// Set command line arguments
	if (opts.w) props.workDir = opts.w
	props.logEncoding = (opts.l) ? opts.l : "UTF-8"
	props.preview = (opts.p) ? 'true' : 'false'

	if ( opts.cr )
		props.codereview_crRulesFile = opts.cr

	if ( opts.ccr )
		props.codereview_ccrRulesFile = opts.ccr

	// Validate required properties
	try {
		assert props.workDir: "Missing commandline property - workDir - build work directory"
		assert props.codereview_jobcard: "Missing property in properties file - codereview_jobcard - jobcard"
		assert props.codereview_crRulesFile: "Missing property in properties file - codereview_crRulesFile - code review rules file"
		assert props.codereview_includedFiles: "Missing property in properties file - codereview_includedFiles - included files filter"
		assert props.codereview_includedIncludeFiles: "Missing property in properties file - codereview_includedIncludeFiles - reference to syslib datasets"
	} catch (AssertionError e) {
		cli.usage()
		throw e
	}
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

def formatJCLPath(String line)
{
	int len = line.length();
	List<String> result = new ArrayList<String>();
	int offset= 0;
	int i = 0;
	int j = 0;
	while (i < len)
	{
		if ( offset == 71 ) {
			result.add(line.substring(j,i));
			j=i;
			offset=16;
		} else {
			offset++;
		}
		i++;
	}
	if ( j < i )
		result.add(line.substring(j,i));
	for ( i=1; i < result.size(); i++ ) {
		result.set(i, "//             " + result.get(i));
	}
	return result;
}

