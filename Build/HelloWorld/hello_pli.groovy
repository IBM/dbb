import com.ibm.dbb.build.*

// Change the following variables to match your system
hlq        = "USER.BUILD"
sourceDir  = "/u/user/build"
compilerDS = "IBMZ.V5R2M0.SIBMZCMP"

println("Creating ${hlq}.PLI. . .")
CreatePDS createPDSCmd = new CreatePDS();
createPDSCmd.setDataset("${hlq}.PLI");
createPDSCmd.setOptions("tracks space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library)");
createPDSCmd.create();

println("Creating ${hlq}.OBJ. . .")
createPDSCmd.setDataset("${hlq}.OBJ");
createPDSCmd.setOptions("tracks space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library)");
createPDSCmd.create();

println("Copying ${sourceDir}/hello.pli to ${hlq}.PLI(HELLO) . . .")
def copy = new CopyToPDS().file(new File("${sourceDir}/hello.pli")).dataset("${hlq}.PLI").member("HELLO")
copy.execute()

println("Compiling ${hlq}.PLI(HELLO). . .")
def compile = new MVSExec().pgm("IBMZPLI").parm("SOURCE")
compile.dd(new DDStatement().name("SYSIN").dsn("${hlq}.PLI(HELLO)").options("shr"))
compile.dd(new DDStatement().name("SYSLIN").dsn("${hlq}.OBJ(HELLO)").options("shr"))
compile.dd(new DDStatement().name("SYSUT1").options("cyl space(5,5) unit(vio)  new"))
compile.dd(new DDStatement().name("SYSTERM").options("cyl space(5,5) unit(vio)  new"))
compile.dd(new DDStatement().name("SYSPUNCH").options("cyl space(5,5) unit(vio)  new"))
compile.dd(new DDStatement().name("SYSOUT").options("cyl space(5,5) unit(vio)  new"))
compile.dd(new DDStatement().name("TASKLIB").dsn("${compilerDS}").options("shr"))
compile.dd(new DDStatement().name("SYSPRINT").options("cyl space(5,5) unit(vio)  new"))
compile.copy(new CopyToHFS().ddName("SYSPRINT").file(new File("${sourceDir}/hello_pli.log")))
def rc = compile.execute()

if (rc > 4)
    println("Compile failed!  RC=$rc")
else
    println("Compile successful!  RC=$rc")
