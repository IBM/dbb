@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.build.*
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import groovy.transform.*

/**
 * This script builds a DB2 application package for SQL programs in the application.
 */

bind(args)

def bindPackage(String file, String dbrmHLQ, String workDir, String confDir, String SUBSYS, String COLLID, String OWNER, String QUAL, boolean verbose) {
	// define local properties
	def member = CopyToPDS.createMemberName(file)
	def logFile = new File("${workDir}/${member}_bind.log")
	println("*** Binding $file")

	String jobstmts = """
//DBBBND JOB 'DBB-PKGBIND',REGION=0M,MSGLEVEL=1,MSGCLASS=1,     
//  CLASS=1,SCHENV=DB2@${SUBSYS},LINES=(10000,WARNING)      
//******************************************************************* 
//* DESCRIPTION: BIND DB2 PACKAGE                                   * 
//JOBLIB   DD  DISP=SHR,
//             DSN=DB2A.DSNEXIT
//         DD  DISP=SHR,
//             DSN=DB2A.DSNLOAD
//*******************************************
//* PKGBIND
//* Step bind packages
//*******************************************
//**BEGIN
//PKGBIND EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT)
//SYSTSPRT DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSIN    DD  DUMMY
//SYSTSIN  DD  *
	DSN SYSTEM(${SUBSYS})   
	   BIND PACKAGE(${COLLID})    +                                
       MEMBER(${member})     +
       OWNER(${OWNER})       +                                
       QUALIFIER(${QUAL})    +                                
       ACTION(REPLACE)     +   
       LIBRARY('${dbrmHLQ}')
       ISOLATION(CS)                                        
  END                                                       
"""

	def exec = new JCLExec()
	int jrc = exec.text(jobstmts).confDir(confDir).execute()
	int rc = 16
	def jobRc= exec.maxRC
	if (jobRc.find('CC')){
		rc = jobRc.split("CC")[1].toInteger()
	}
	println "*** Bind Job ${exec.submittedJobId} completed with $jobRc"
	exec.saveOutput(logFile)
	
	return [rc,"${workDir}/${member}_bind.log"]

 }

//Parse the command line and bind
def bind(String[] cliArgs)
{
	def cli = new CliBuilder(usage: "BindUtilities.groovy [options]", header: '', stopAtNonOption: false)
	cli.f(longOpt:'file', args:1, required:true, 'The build file name.')
	cli.d(longOpt:'dbrmHLQ', args:1, required:true, 'DBRM partition data sets')	
	cli.w(longOpt:'workDir', args:1, required:true, 'Absolute path to the working directory')
	cli.c(longOpt:'confDir', args:1, required:true, 'Absolute path to runIspf.sh folder')
	
	cli.s(longOpt:'subSys', args:1, required:true, 'The name of the DB2 subsystem')
	cli.p(longOpt:'collId', args:1, required:true, 'Specify the DB2 collection (Package)')
	cli.o(longOpt:'owner', args:1, required:true, 'The owner of the package')
	cli.q(longOpt:'qual', args:1, required:true, 'The value of the implicit qualifier')
	cli.m(longOpt:'maxRc', args:1, 'The maximun return value')
	
	cli.v(longOpt:'verbose', 'Flag to turn on script trace')
	
	def opts = cli.parse(cliArgs)
	
	// if opt parse fail exit.
	if (! opts) {
		System.exit(1)
	}
	
	if (opts.help)
	{
		cli.usage()
		System.exit(0)
	}
	
	def maxRC = opts.m ? opts.m.toInteger() : 0
	def (rc, logFile) = bindPackage(opts.f, opts.d, opts.w, opts.c, opts.s, opts.p, opts.o, opts.q, opts.v)
	if ( rc > maxRC ) {
		String errorMsg = "*! The bind return code ($rc) for $opts.f exceeded the maximum return code allowed ($maxRC)\n** See: $logFile"
		println(errorMsg)
		System.exit(1)
	}
	if ( rc <= maxRC ) {
		String errorMsg = "*! Bind was successful with a return code ($rc). See: $logFile"
		println(errorMsg)
		System.exit(0)
	}
}


def bindPlan(String file, String workDir, String confDir, String SUBSYS, String COLLID, String OWNER, String QUAL, boolean verbose) {
	// define local properties
	def member = CopyToPDS.createMemberName(file)
	def logFile = new File("${workDir}/${member}_plan.log")
	println("*** Binding $file")

	String jobstmts = """
//DBBPLN JOB 'DBB-PLNBIND',REGION=0M,MSGLEVEL=1,MSGCLASS=1,     
//  CLASS=1,SCHENV=DB2@${SUBSYS},LINES=(10000,WARNING)      
//******************************************************************* 
//* DESCRIPTION: BIND DB2 PLAN                                   * 
//JOBLIB   DD  DISP=SHR,
//             DSN=DB2A.DSNEXIT
//         DD  DISP=SHR,
//             DSN=DB2A.DSNLOAD
//*******************************************
//* PKGBIND
//* Step bind packages
//*******************************************
//**BEGIN
//PKGBIND EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT)
//SYSTSPRT DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*
//SYSIN    DD  DUMMY
//SYSTSIN  DD  *                                                       
	DSN SYSTEM(${SUBSYS})   
	BIND PLAN (${member})           +
    OWNER(${OWNER})                 +
    PKLIST (*.${COLLID}.*)          +
    ACTION (REPLACE)                +
    VALIDATE(BIND)                  +
    ISOLATION(CS)                   +
    RELEASE (COMMIT)                +
    CURRENTDATA(NO)                 +
    EXPLAIN (YES)                   +
    RETAIN
END
                                                    
"""

	def exec = new JCLExec()
	int jrc = exec.text(jobstmts).confDir(confDir).execute()
	int rc = 16
	def jobRc= exec.maxRC
	if (jobRc.find('CC')){
		rc = jobRc.split("CC")[1].toInteger()
	}
	println "*** Bind Job ${exec.submittedJobId} completed with $jobRc"
	exec.saveOutput(logFile)
	
	return [rc,"${workDir}/${member}_plan.log"]

 }

//Parse the command line and bind
// dbrm not required for plan
def bindP(String[] cliArgs)
{
	def cli = new CliBuilder(usage: "BindUtilities.groovy [options]", header: '', stopAtNonOption: false)
	cli.f(longOpt:'file', args:1, required:true, 'The build file name.')
	cli.w(longOpt:'workDir', args:1, required:true, 'Absolute path to the working directory')
	cli.c(longOpt:'confDir', args:1, required:true, 'Absolute path to runIspf.sh folder')
	
	cli.s(longOpt:'subSys', args:1, required:true, 'The name of the DB2 subsystem')
	cli.p(longOpt:'collId', args:1, required:true, 'Specify the DB2 collection (Package)')
	cli.o(longOpt:'owner', args:1, required:true, 'The owner of the package')
	cli.q(longOpt:'qual', args:1, required:true, 'The value of the implicit qualifier')
	cli.m(longOpt:'maxRc', args:1, 'The maximun return value')
	
	cli.v(longOpt:'verbose', 'Flag to turn on script trace')
	
	def opts = cli.parse(cliArgs)
	
	// if opt parse fail exit.
	if (! opts) {
		System.exit(1)
	}
	
	if (opts.help)
	{
		cli.usage()
		System.exit(0)
	}
	
	def maxRC = opts.m ? opts.m.toInteger() : 0
	def (rc, logFile) = bindPlan(opts.f, opts.w, opts.c, opts.s, opts.p, opts.o, opts.q, opts.v)
	if ( rc > maxRC ) {
		String errorMsg = "*! The bind return code ($rc) for $opts.f exceeded the maximum return code allowed ($maxRC)\n** See: $logFile"
		println(errorMsg)
		System.exit(1)
	}
	if ( rc <= maxRC ) {
		String errorMsg = "*! Bind was successful with a return code ($rc). See: $logFile"
		println(errorMsg)
		System.exit(0)
	}
}

