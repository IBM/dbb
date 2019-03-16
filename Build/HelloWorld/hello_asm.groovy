import com.ibm.dbb.build.*

// copy program to PDS
println("Copying hello.asm to USER.HASRC")
new CopyToPDS().file(new File("hello.asm")).dataset("USER.HASRC").member("HELLO").execute()

// compile the build file
println("Compiling hello.asm")

// define the MVSExec command to compile the file
def compile = new MVSExec().file("hello.asm").pgm("ASMA90").parm("")

// add DD statements to the MVSExec command
compile.dd(new DDStatement().name("SYSIN").dsn("USER.HASRC(HELLO)").options("shr").report(true))
compile.dd(new DDStatement().name("SYSLIN").dsn("USER.HAMOD(HELLO)").options("shr").output(true))
compile.dd(new DDStatement().name("SYSPRINT").options("cyl space(5,5) unit(vio) new"))
compile.dd(new DDStatement().name("SYSUT1").options("cyl space(5,5) unit(vio) new"))

// add a syslib to the compile command with optional CICS concatenation
compile.dd(new DDStatement().name("SYSLIB").dsn("USER.HAMAC").options("shr"))
compile.dd(new DDStatement().dsn("SYS1.MACLIB").options("shr"))

// log 
compile.copy(new CopyToHFS().ddName("SYSPRINT").file(new File("/u/user/build/hello.log")))

// execute the MVSExec compile command
def rc = compile.execute()

if (rc > 4)
    println("Compile failed!  RC=$rc")
else
    println("Compile successful!  RC=$rc")
