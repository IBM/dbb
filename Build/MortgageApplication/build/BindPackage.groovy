@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.build.*
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*

/**
* This script builds a DB2 application package for SQL programs in the MortgageApplication sample.  It is 
* called by the CobolCompile.groovy script.  It requires that the properties in MortgageApplication/build/bind.properties 
* be set to the correct values for your system.
*
* **NOTE - This script contains syntax that requires PTF UI54779 be applied to the DBB toolkit. 
*/

// receive passed arguments
def file = argMap.file
println("* Binding $file using ${this.class.getName()}.groovy script")

// define local properties
def properties = BuildProperties.getInstance()
def dbrmPDS = "${properties.hlq}.DBRM"
def clistPDS = "${properties.hlq}.CLIST"
def cmdscpDS = "${properties.hlq}.ISPFGWY.EXEC"
def member = CopyToPDS.createMemberName(file)
def logFile = new File("${properties.workDir}/${member}_bind.log")
def srcOptions = "cyl space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library) msg(1)"

// create BIND CLIST if necessary
def clist = new File("${properties.workDir}/bind.clist")
if (!clist.exists()) {
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
   println("Copying ${properties.workDir}/bind.clist to $clistPDS(BIND)")
   new CopyToPDS().file(clist).dataset(clistPDS).member("BIND").execute()
}

// bind the build file
println("Executing CLIST to bind program $file")	

// define TSOExec to run the bind clist
def bind = new TSOExec().file(file)
                        .command("exec '$clistPDS(BIND)'")
                        .options("'${properties.SUBSYS} ${properties.COLLID} $member $dbrmPDS ${properties.OWNER} ${properties.QUAL}'")
                        .logFile(logFile)
                        .confDir(properties.CONFDIR)
                        .keepCommandScript(true)
bind.dd(new DDStatement().name("CMDSCP").dsn(cmdscpDS).options("shr"))

// execute the bind clist
def rc = bind.execute()

// add the DB2 BIND status to the build result
def tools = loadScript(new File("Tools.groovy"))
tools.updateBuildResult(file:"$file", rc:rc, maxRC:0, log:logFile)
