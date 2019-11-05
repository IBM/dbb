import com.ibm.dbb.build.*

// Change the following variables to match your system
hlq        = "USER"
sourceDir  = "/u/user/build"
macLib     = "SYS1.MACLIB"
//userMacLib = "USER.MACLIB"  //Uncomment if needed

println("Creating ${hlq}.HASRC. . .")
CreatePDS createPDSCmd = new CreatePDS();
createPDSCmd.setDataset("${hlq}.HASRC");
createPDSCmd.setOptions("tracks space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library)");
createPDSCmd.create();

println("Creating ${hlq}.HAMOD. . .")
createPDSCmd.setDataset("${hlq}.HAMOD");
createPDSCmd.setOptions("tracks space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library)");
createPDSCmd.create();

// copy program to PDS
println("Copying hello.asm to ${hlq}.HASRC")
new CopyToPDS().file(new File("${sourceDir}/hello.asm")).dataset("${hlq}.HASRC").member("HELLO").execute()

// assemble the build file
println("Assembling hello.asm")

// define the MVSExec command to compile the file
def compile = new MVSExec().file("${sourceDir}/hello.asm").pgm("ASMA90").parm("")

// add DD statements to the MVSExec command
compile.dd(new DDStatement().name("SYSIN").dsn("${hlq}.HASRC(HELLO)").options("shr").report(true))
compile.dd(new DDStatement().name("SYSLIN").dsn("${hlq}.HAMOD(HELLO)").options("shr").output(true))
compile.dd(new DDStatement().name("SYSPRINT").options("cyl space(5,5) unit(vio) new"))
compile.dd(new DDStatement().name("SYSUT1").options("cyl space(5,5) unit(vio) new"))

// add a syslib to the compile command with optional CICS concatenation
compile.dd(new DDStatement().name("SYSLIB").dsn("${macLib}").options("shr"))
//compile.dd(new DDStatement().dsn("${userMacLib}").options("shr"))   //Uncomment if user maclib needed

// log 
compile.copy(new CopyToHFS().ddName("SYSPRINT").file(new File("${sourceDir}/hello_asm.log")))

// execute the MVSExec compile command
def rc = compile.execute()

if (rc > 4)
    println("Assembly failed!  RC=$rc")
else
    println("Assembly successful!  RC=$rc")
