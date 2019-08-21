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
	def dbrmPDS = "${dbrmHLQ}"
	def clistPDS = "${dbrmHLQ}.CLIST"
	def cmdscpDS = "${dbrmHLQ}.ISPFGWY.EXEC"
	def member = CopyToPDS.createMemberName(file)
	def logFile = new File("${workDir}/${member}_bind.log")
	def srcOptions = "cyl space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library) msg(1)"


	println("*** Binding $file")


	// create BIND CLIST if necessary
	def clist = new File("${workDir}/bind.clist")
	if (clist.exists()) {
		clist.delete()
	}
	
	clist << """PROC 6 SUBSYS COLLID MEMBER LIB OWNER QUAL                       
   DSN SYSTEM(&SUBSYS)                                       
   BIND PACKAGE(&COLLID)    +                                
        MEMBER(&MEMBER)     +
        LIBRARY('&LIB')     +                                
        OWNER(&OWNER)       +                                
        QUALIFIER(&QUAL)    +                                
        ACTION(REPLACE)     +                                
        ISOLATION(CS)                                        
   END                                                       
EXIT CODE(&LASTCC)
"""

	// create CLIST PDS if necessary
	new CreatePDS().dataset(clistPDS).options(srcOptions).create()

	// copy CLIST to PDS
	if ( verbose )
		println("*** Copying ${workDir}/bind.clist to $clistPDS(BIND)")
	new CopyToPDS().file(clist).dataset(clistPDS).member("BIND").execute()

	// bind the build file
	if ( verbose )
		println("*** Executing CLIST to bind program $file")

	// define TSOExec to run the bind clist
	def bind = new TSOExec().file(file)
			.command("exec '$clistPDS(BIND)'")
			.options("'${SUBSYS} ${COLLID} $member $dbrmPDS ${OWNER} ${QUAL}'")
			.logFile(logFile)
			.confDir(confDir)
			.keepCommandScript(true)
	bind.dd(new DDStatement().name("CMDSCP").dsn(cmdscpDS).options("shr"))

	// execute the bind clist
	def rc = bind.execute()
	
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
}

