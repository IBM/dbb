@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
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
 *  	-w,--workDir <dir>					Absolute path to the DBB build output directory
 *  	-l,--logEncoding					(Optional) Defines the Encoding for output files (JCL spool, reports), default UTF-8
 *  	-props,--properties					(Optional) Absolute path to the codereview.properties file
 *  	-p,--preview						(Optional) Preview JCL, do not submit it
 *  	-h,--help							(Optional) Prints this message
 *  	-ccr,--ccrRulesFile <arg>   		(Optional) Absolute path of the custom rules file.
 *  										If not provided, will look for it in the codereview.properties file.
 *  	-cp,--codepage <arg>        		(Optional) Code Page of the source  members to be processed.
 *  										By default, IBM-037 is used by Code Review if none is specified.
 *  										If not provided, will look for it in the codereview.properties file.
 *  	-cr,--crRulesFile <arg>     		(Optional) Absolute path of the rules file.
 *  										If not provided, will look for it in the codereview.properties file.
 *  	-pgFile,--propertyGroupFile <arg>   Absolute path of the Property Group file. This file has to be a Local Property Group file.
 *  										Optional if SYSLIB concatenation is found by the script, otherwise required.
 *  	-rc,--maxRC <arg>           		(Optional) Maximum acceptable return code.
 *  										If not provided, will look for it in the codereview.properties file.
 *
 * 	requires:
 * 		codeview.properties file - externalizes the JCL jobcard, RuleFile and Mappings.
 * 		If --properties not provided via cli, the script looks for it at the location of the script itself
 *
 */

@Field BuildProperties props = BuildProperties.getInstance()
parseInput(args)

def startTime = new Date()
props.startTime = startTime.format("yyyyMMdd.HHmmss.SSS")
println("** Run Code Review started at $props.startTime")
println("** Properties at startup:")
props.each { k,v ->
	println "   $k -> $v"
}

// read build report data
println("** Read build report data from '$props.workDir/BuildReport.json'")
def jsonOutputFile = new File("${props.workDir}/BuildReport.json")

if (!jsonOutputFile.exists()) {
	println("!* [Error] Build Report not found at '$props.workDir/BuildReport.json'. Exiting.")
	System.exit(1)
}

def buildReport = BuildReport.parse(new FileInputStream(jsonOutputFile))

// Parse build report to find the build outputs to be deployed.
// The following example finds all the source code with the provided file extension
List<PathMatcher> fileFilter = createIncludePatterns(props.codereview_includedFiles)
def sources = buildReport.getRecords().findAll {
	it.getType() == DefaultRecordFactory.TYPE_COPY_TO_PDS && matches(it.getSource().getAbsolutePath(), fileFilter)
}

// The following example finds all the source code with the provided file extension
List<PathMatcher> fileIncludeFilter = createIncludePatterns(props.codereview_includedIncludeFiles)
def includes= buildReport.getRecords().findAll {
	it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS && matches(it.getSource().getAbsolutePath(), fileIncludeFilter)
}

if (sources.size == 0) {
	println("!* [Warning] No source files found in the Build Report. Skipping Code Review.")
	System.exit(0)
}
println("** Found source code processed in the build report:")
sources.each {
	println("   ${it.getSource()}, ${it.getDestination()}")
}

println("** Found include files processed in the build report to extract SYSLIB.")

// List to collect the Copybook libraries for the syslib
List syslib = new ArrayList<String>()
includes.each {
	println("   ${it.getSource()}, ${it.getDestination()}")
	syslib.add(it.getDestination().take(it.getDestination().indexOf("(")))
}
syslib = syslib.toUnique()

//If no SYSLIB found and no SYSLIB passed in codereview_syslib and no Property Group file passed, fails and exits
if (syslib.size() == 0 && !props.codereview_syslib && !props.codereview_propertyGroupFile) {
	println("!* [Error] SYSLIB concatenation is empty and no Property Group file was provided. Exiting.")
	System.exit(2)
}

println("** Create JCL Stream for IDZ Code Review")
String jobcard = props.codereview_jobcard.replace("\\n", "\n")
JobExec codeReview = createCodeReviewExec(jobcard, props.codereview_crRulesFile, props.codereview_ccrRulesFile, props.codereview_propertyGroupFile, sources, syslib)

if (props.preview.toBoolean()) {
	println "** Preview only."
} else {
	// Execute jclExec
	println("** Execute IDZ Code Review Application via JCL.")
	codeReview.execute()

	// Retrieve the maxRC from property
	int codeReview_maxRC
	if (props.codereview_maxRC) {
		codeReview_maxRC = props.codereview_maxRC.toInteger()
		println("*** Maximum acceptable return code is ${props.codereview_maxRC}")
	} else {
		codeReview_maxRC = 3
		println("*** Maximum acceptable return code is 3 (default)")
	}

	// Splitting the String into a StringArray using CC as the separator
	def jobRcStringArray = codeReview.maxRC.split("CC")
	// This evals the number of items in the ARRAY! Don't get confused with the returnCode itself
	if (jobRcStringArray.length > 1){
		// Ok, the string can be splitted because it contains the keyword CC : Splitting by CC the second record contains the actual RC
		rc = jobRcStringArray[1].toInteger()
		// manage processing the RC, up to your logic. You might want to flag the build as failed.
		if (rc <= codeReview_maxRC) {
			println("*** Job '${codeReview.submittedJobId}' completed with RC=$rc")
		} else {
			println("*** Job '${codeReview.submittedJobId}' failed with RC=$rc")
		}
		
		println("** Saving spool output to ${props.workDir}")
		def logFile = new File("${props.workDir}/CodeReviewSpool-${codeReview.getSubmittedJobId()}.txt")
		codeReview.saveOutput(logFile, props.logEncoding)

		codeReview.getAllDDNames().each() { ddName ->
			if (ddName.equals("XML") || ddName.equals("JUNIT") || ddName.equals("CSV")) {
				def ddFile = new File("${props.workDir}/CodeReview-${ddName}.xml")
				codeReview.saveOutput(ddName, ddFile, props.logEncoding)
			}
		}

		if (rc > codeReview_maxRC) {
			System.exit(rc)
		} else {
			System.exit(0)
		}
	} else {
		// We don't see the CC, assume an failure
		println("*** Job ${codeReview.submittedJobId} failed with ${codeReview.maxRC}")
		System.exit(codeReview.maxRC)
	}
}

/*
 * CodeReviewExec - creates a JCLExec command for CodeReview
 * TODO: Externalize SYSLIB
 */
def createCodeReviewExec(String jobcard, String ruleFile, String customRuleFile, String propertyGroupFile, List memberList, List syslib) {
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
	if (props.codereview_syslib) {
		props.codereview_syslib.split(',').each {
			jcl+="//         DD DISP=SHR,DSN=${it} \n"
		}
	}

	if (propertyGroupFile) {
		def lines = formatJCLPath("//PROPERTY  DD PATH='$propertyGroupFile'")
		lines.each{ jcl += it + "\n" }
	}
		
	if (customRuleFile) {
		def lines = formatJCLPath("//CUSTRULE  DD PATH='$customRuleFile'")
		lines.each {
			jcl += it + "\n"
		}
	}
	def lines = formatJCLPath("//RULES  DD PATH='$ruleFile'")
	lines.each {
		jcl += it + "\n"
	}

	jcl += "//LIST   DD *\n"
	def languageMapping = new PropertyMappings("codereview_languageMapping")
	memberList.each {
		hfsFile = CopyToPDS.createMemberName(it.getSource().getName())
		pdsMember = it.getDestination().take(it.getDestination().indexOf("("))
		if (languageMapping.isMapped("COBOL", it.getSource().getAbsolutePath()))
			jcl += "  L=COBOL M=$hfsFile D=$pdsMember \n"
		if (languageMapping.isMapped("PLI", it.getSource().getAbsolutePath()))
			jcl += "  L=PL/1 M=$hfsFile D=$pdsMember \n"
	}

	if (props.codereview_codepage) {
		jcl += """\
//CODEPAGE DD *
  $props.codereview_codepage
"""
	}

	jcl += """\
//CSV DD SYSOUT=*,RECFM=VB,LRECL=2051
//XML DD SYSOUT=*,RECFM=VB,LRECL=2051
//MSGS DD SYSOUT=*,RECFM=VB,LRECL=2051
//*
//
"""
	println(jcl)

	// Create jclExec
	def codeReview = new JobExec().text(jcl)
	return codeReview
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
	cli.pgFile(longOpt:'propertyGroupFile', args:1, 'Absolute path of the Property Group file. Optional if SYSLIB concatenation is found by the script, otherwise required.')
	cli.cp(longOpt:'codepage', args:1, '(Optional) Code Page of the source members to be processed. By default, IBM-037 is used by Code Review if none is specified.')
	cli.l(longOpt:'logEncoding', args:1, '(Optional) Defines the Encoding for output files (JCL spool, reports), default UTF-8')
	cli.p(longOpt:'preview', '(Optional) Preview JCL for Code Review, do not submit it')
	cli.rc(longOpt:'maxRC', args:1, '(Optional) Maximum acceptable return code. If not provided, will look for it in the codereview.properties file.')
	cli.h(longOpt:'help', '(Optional) Prints this message.')

	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
		cli.usage()
		System.exit(0)
	}

	// Identify config file
	if (opts.props) { // if property file is supplied via cli
		buildPropFile = new File(opts.props)
	} else { // looking at the default location
		def scriptDir = getScriptDir()
		buildPropFile = new File("$scriptDir/codereview.properties")
	}
	// Import properties from config file
	if (buildPropFile.exists()) {
		props.buildPropFile = buildPropFile.getAbsolutePath()
		props.load(buildPropFile)
	} else {
		println("!* [Error] Property File 'codereview.properties' not found. Exiting.")
		System.exit(1)
	}

	// Set command line arguments
	if (opts.w)
		props.workDir = opts.w
	props.logEncoding = (opts.l) ? opts.l : "UTF-8"
	props.preview = (opts.p) ? 'true' : 'false'

	if (opts.cr)
		props.codereview_crRulesFile = opts.cr

	if (opts.ccr)
		props.codereview_ccrRulesFile = opts.ccr

	if (opts.pgFile)
		props.codereview_propertyGroupFile = opts.pgFile

	if (opts.cp)
		props.codereview_codepage = opts.cp
	
	if (opts.rc)
		props.codereview_maxRC = opts.rc

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
		if (matcher.matches(path)) {
			return true
		}
	}
	return result
}

def formatJCLPath(String line) {
	int len = line.length();
	List<String> result = new ArrayList<String>();
	int offset= 0;
	int i = 0;
	int j = 0;
	while (i < len)	{
		if (offset == 71) {
			result.add(line.substring(j,i));
			j = i;
			offset = 16;
		} else {
			offset++;
		}
		i++;
	}
	if (j < i)
		result.add(line.substring(j,i));
	for (i=1; i < result.size(); i++) {
		result.set(i, "//             " + result.get(i));
	}
	return result;
}