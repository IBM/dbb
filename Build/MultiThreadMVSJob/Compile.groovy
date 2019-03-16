import com.ibm.dbb.build.*
import java.util.concurrent.*
import java.util.concurrent.atomic.*

//********************************************************************************
// This simple script is used to compile and linkedit sample Cobol files.  It
// processes file in sequence non-thread environment.  The purpose of having 
// this script in the sample so that we can illustrate how to convert it
// to support multi-thread environment.
// ===============================================================================
// Required Modification:
// 1. Need to change the HLQ variable for the source and output of the PDS.
//********************************************************************************

ElapsedTimer timer = new ElapsedTimer()
timer.start()

//*****************************************
// Define datasets for source and output
//*****************************************
// Sample:  'USER.DBB.TEST'
def HLQ = 
def sourcePDS = "${HLQ}.COBOL"
def objPDS = "${HLQ}.OBJ"
def loadPDS = "${HLQ}.LOAD"

//********************************************************************************
//* Locate the current directory where build script and source files are on zFS
//********************************************************************************
def currentDir = new File(getClass().protectionDomain.codeSource.location.path).parentFile
def cobolDir = new File(currentDir, 'COBOL')
def logDir = new File(currentDir, 'logs')

//********************************************************************************
//Create required PDS 
//********************************************************************************
new CreatePDS().dataset(sourcePDS).options('cyl space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library)').create()
new CreatePDS().dataset(objPDS).options('cyl space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library)').create()
new CreatePDS().dataset(loadPDS).options('cyl space(1,1) dsorg(PO) recfm(U) blksize(32760) dsntype(library) msg(1)').create()

//********************************************************************************
 // Setup a process counter to count the number of files processed.
 //********************************************************************************
def processCounter = 0

//********************************************************************************
// Fetch all source files from zFS to PDS
//********************************************************************************
cobolDir.eachFile { file ->
    
    def member = file.name.take(file.name.lastIndexOf('.'))
    println "Copy '$file' to $sourcePDS($member)"    
    new CopyToPDS().file(file).dataset(sourcePDS).member(member).execute()

    def logFile = new File(logDir, "${member}.log")
    
    //Setup a MVSExec compile step
    def compileStep = new MVSExec().file("$file").pgm('IGYCRCTL').parm('LIB')
    compileStep.dd(new DDStatement().name("SYSIN").dsn("$sourcePDS($member)").options("shr").report(true))
    compileStep.dd(new DDStatement().name("SYSLIN").dsn("$objPDS($member)").options("shr").output(true).pass(true))
    compileStep.dd(new DDStatement().name("SYSUT1").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    compileStep.dd(new DDStatement().name("SYSUT2").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    compileStep.dd(new DDStatement().name("SYSUT3").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    compileStep.dd(new DDStatement().name("SYSUT4").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    compileStep.dd(new DDStatement().name("SYSUT5").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    compileStep.dd(new DDStatement().name("SYSUT6").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    compileStep.dd(new DDStatement().name("SYSUT7").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    compileStep.dd(new DDStatement().name("SYSMDECK").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    compileStep.dd(new DDStatement().name("TASKLIB").dsn('IGY.V4R2M0.SIGYCOMP').options("shr"))
    compileStep.dd(new DDStatement().name("SYSPRINT").options('cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new'))
    compileStep.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile))

    //Setup a MVSExec linkedit step
    def linkeditStep = new MVSExec().file("$file").pgm("IEWBLINK").parm("MAP,RENT,COMPAT(PM5)")
    linkeditStep.dd(new DDStatement().name("SYSLMOD").dsn("$loadPDS($member)").options("shr").output(true).deployType("LOAD"))
    linkeditStep.dd(new DDStatement().name("SYSPRINT").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    linkeditStep.dd(new DDStatement().name("SYSUT1").options("cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"))
    linkeditStep.dd(new DDStatement().name("SYSLIB").dsn(objPDS).options("shr"))
    linkeditStep.dd(new DDStatement().dsn('CEE.SCEELKED').options("shr"))
    linkeditStep.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).append(true))

    //Run the above MVSExecs in a MVSJob
    def job = new MVSJob()
    job.start()

    def rc = compileStep.execute()
    if (rc <= 4)
        rc = linkeditStep.execute()

    println "Result of processing $member: $rc"

    job.stop()
    
    processCounter++
}     

println "Build complete in ${timer.stop()}"
println "Total files processed: $processCounter"

