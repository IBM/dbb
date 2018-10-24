import com.ibm.dbb.build.*
import java.util.concurrent.*
import java.util.concurrent.atomic.*

//********************************************************************************
// This is the multi-thread version of the Compile.groovy. The following changes
// to the Compile.groovy are needed:
//
// 1. Separate the fetch and compile steps.  Perform fetch for all files in
//    the main thread.  Perform the compile steps using a thread pool.
// 2. Setup a thread pool with maximum number of threads allowed.
// 3. Setup a completion service for the thread pool so that we can wait for
//    the completion of each thread.
// 4. The processCounter is changed from Integer to AtomicInteger to support
//    multi-thread to update the counter.
// 5. Include the MVSJob in the thread submit closure.
// 6. Invoke ThreadPool.shutdown after all threads are complete.
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
// Create a thread pool of 2 (maximum of 2 threads will run at once).
//********************************************************************************
int maxPoolSize = 2
ExecutorService threadPool = Executors.newFixedThreadPool(maxPoolSize)

//********************************************************************************
// Setup a CompletionService so that we can track the completion of the thread.
// The purpose of this is to ensure that we count the number of files processed
// correctly, and also to ensure that any post-processing is performed only
// after all threads have finished, ie: shutting down the thread pool.
//********************************************************************************
ExecutorCompletionService<Boolean> completionService = new ExecutorCompletionService<Boolean>(threadPool)

//********************************************************************************
// Setup a process counter to count the number of files processed.
//********************************************************************************
def processCounter = new AtomicInteger()

//********************************************************************************
// Total number of files to be processed.
//********************************************************************************
def totalCounter = 0

//********************************************************************************
// Fetch all source files from zFS to PDS
//********************************************************************************
println 'Start fetch...'
cobolDir.eachFile { file ->
    
    def member = file.name.take(file.name.lastIndexOf('.'))
    println "Copy '$file' to $sourcePDS($member)"    
    new CopyToPDS().file(file).dataset(sourcePDS).member(member).execute()
    totalCounter++
    
}
println "Complete fetch in ${timer.pause()}"

//********************************************************************************
// For each file, setup a MVSJob to include compile and linkedit steps and
// execute each MVSJob in a thread when there is available thread in the pool.
//********************************************************************************
println 'Start compile...'
cobolDir.eachFile { file ->
    
    def member = file.name.take(file.name.lastIndexOf('.'))
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
    
    //Submit the MVSJob that includes the above 2 MVSExecs to run in a thread
    completionService.submit {

        def job = new MVSJob()
        try
        {                        
            job.start()

            def rc = compileStep.execute()
            if (rc <= 4)
                rc = linkeditStep.execute()

            println "Result of processing $member: $rc"
        }
        catch (Exception e)
        {
            e.printStackTrace()
            return false
        }
        finally
        {
            job.stop()
        }
        return true
    }
}

//********************************************************************************
// Block and wait for all threads to complete.  Notice that threads can finish
// not in the same order as they are started.  As each thread completed,
// increment the counter.
//********************************************************************************
boolean success = true
for (int i=0; i<totalCounter; i++)
{
    Future<Boolean> status = completionService.take()
    boolean rc = status.get()
    if (!rc)
        success = rc   
    processCounter.incrementAndGet()
}

println "Complete compile in ${timer.pause()}"  
println "Total files processed: $processCounter"
if (success)
    println "Build Completed SUCCESSFULLY"
else
    println "Build Completed with ERROR"    

//********************************************************************************
// This is required to clean up all resources allocated for each thread in
// the pool.
//********************************************************************************
threadPool.shutdown()

println "Build completed in ${timer.stop()}"
