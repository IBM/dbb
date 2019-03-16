@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*

// receive passed arguments
def file = argMap.file
println("* Building $file using ${this.class.getName()}.groovy script")

// define local properties
def properties = BuildProperties.getInstance()
def mfsPDS = "${properties.hlq}.MFS"
def tformatPDS = "${properties.hlq}.TFORMAT"
def member = CopyToPDS.createMemberName(file)
def logFile = new File("${properties.workDir}/${member}.log")

// create a reference to the Tools.groovy utility script
def tools = loadScript(new File("Tools.groovy"))

// define the BPXWDYN options for allocated temporary datasets
def tempCreateOptions = "tracks space(5,5) unit(vio) new"

// copy program to PDS 
println("Copying ${properties.sourceDir}/$file to $mfsPDS($member)")
new CopyToPDS().file(new File("${properties.sourceDir}/$file")).dataset(mfsPDS).member(member).execute()

// Generate the MFS program
println("Generating MFS program $file")	

// define the MVSExec command for MFS Language Utility - Phase 1
def parms = "NOXREF,NOCOMP,NOSUBS,NODIAG,NOCOMPRESS,LINECNT=55,STOPRC=8,DEVCHAR=I"
def mfsPhase1 = new MVSExec().file(file).pgm("DFSUPAA0").parm(parms)

// add DD statements to the mfsPhase1 command
mfsPhase1.dd(new DDStatement().name("SYSIN").dsn("$mfsPDS($member)").options("shr").report(true))
mfsPhase1.dd(new DDStatement().name("REFIN").dsn(properties.REFERAL).options("shr"))
mfsPhase1.dd(new DDStatement().name("REFOUT").dsn("&&TEMPPDS").options("$tempCreateOptions dir(5) lrecl(80) recfm(f,b)"))
mfsPhase1.dd(new DDStatement().name("REFRD").dsn(properties.REFERAL).options("shr"))
mfsPhase1.dd(new DDStatement().name("SYSTEXT").options(tempCreateOptions))
mfsPhase1.dd(new DDStatement().name("SYSPRINT").options(tempCreateOptions))
mfsPhase1.dd(new DDStatement().name("SEQBLKS").dsn("&&SEQBLK").options(tempCreateOptions).pass(true))
mfsPhase1.dd(new DDStatement().name("SYSLIB").dsn(properties.SDFSMAC).options("shr"))
mfsPhase1.dd(new DDStatement().name("TASKLIB").dsn(properties.SDFSRESL).options("shr"))

// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
mfsPhase1.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(properties.logEncoding))

// define the MVSExec command for MFS Language Utility - Phase 2
def mfsPhase2 = new MVSExec().file(file).pgm("DFSUNUB0").parm("TEST")

// add DD statements to the mfsPhase2 command
mfsPhase2.dd(new DDStatement().name("UTPRINT").options(tempCreateOptions))
mfsPhase2.dd(new DDStatement().name("FORMAT").dsn(tformatPDS).options("shr").output(true))
mfsPhase2.dd(new DDStatement().name("TASKLIB").dsn(properties.SDFSRESL).options("shr"))

// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
mfsPhase2.copy(new CopyToHFS().ddName("UTPRINT").file(logFile).hfsEncoding(properties.logEncoding).append(true))

// execute a simple MVSJob to handle passed temporary DDs between MVSExec commands
def rc = new MVSJob().executable(mfsPhase1).executable(mfsPhase2).maxRC(8).execute()

// update build result
tools.updateBuildResult(file:"$file", rc:rc, maxRC:8, log:logFile)
