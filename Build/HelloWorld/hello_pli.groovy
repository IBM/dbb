import com.ibm.dbb.build.*

println("Copying /u/usr1/build/hello.pli to USR1.BUILD.PLI(HELLO) . . .")
def copy = new CopyToPDS().file(new File("/u/usr1/build/hello.pli")).dataset("USR1.BUILD.PLI").member("HELLO")
copy.execute()

println("Compiling USR1.BUILD.PLI(HELLO). . .")
def compile = new MVSExec().pgm("IBMZPLI").parm("SOURCE")
compile.dd(new DDStatement().name("SYSIN").dsn("USR1.BUILD.PLI(HELLO)").options("shr"))
compile.dd(new DDStatement().name("SYSLIN").dsn("USR1.BUILD.OBJ(HELLO)").options("shr"))
compile.dd(new DDStatement().name("SYSUT1").options("cyl space(5,5) unit(vio)  new"))
compile.dd(new DDStatement().name("SYSTERM").options("cyl space(5,5) unit(vio)  new"))
compile.dd(new DDStatement().name("SYSPUNCH").options("cyl space(5,5) unit(vio)  new"))
compile.dd(new DDStatement().name("SYSOUT").options("cyl space(5,5) unit(vio)  new"))
compile.dd(new DDStatement().name("TASKLIB").dsn("IBMZ.V5R2M0.SIBMZCMP").options("shr"))
compile.dd(new DDStatement().name("SYSPRINT").options("cyl space(5,5) unit(vio)  new"))
compile.copy(new CopyToHFS().ddName("SYSPRINT").file(new File("/u/usr1/build/hello_pli.log")))
def rc = compile.execute()

if (rc > 4)
    println("Compile failed!  RC=$rc")
else
    println("Compile successful!  RC=$rc")
