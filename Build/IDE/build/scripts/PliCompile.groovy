import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*

// receive passed arguments
def file = args[0]
println("* Building $file using ${this.class.getName()}.groovy script")

// define local properties
def properties = BuildProperties.getInstance()
def srcPDS = "${properties.hlq}.PLILIB"
def copyPDS = "${properties.hlq}.PLILIB"
def objectPDS = "${properties.hlq}.PLI"
def member = CopyToPDS.createMemberName(file)
def logFile = new File("${properties.workDir}/${member}.log")

// create a reference to the Tools.groovy utility script
File scriptFile = new File("$properties.sourceDir/Build/IDE/build/Tools.groovy")
Class groovyClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(scriptFile)
GroovyObject tools = (GroovyObject) groovyClass.newInstance()

// define the BPXWDYN options for allocated temporary datasets
def tempCreateOptions = "cyl space(5,5) unit(vio) new"

// copy program to PDS
println("Copying ${properties.sourceDir}/$file to $srcPDS($member)")
new CopyToPDS().file(new File("${properties.sourceDir}/$file")).dataset(srcPDS).member(member).execute()

//resolve program dependencies and copy to PDS
println("Resolving dependencies for file $file and copying to $copyPDS")
def resolver = tools.getPLIDependencyResolver(file)
def deps = resolver.resolve()
new CopyToPDS().dependencies(deps).dataset(copyPDS).execute()

// compile the build file
println("Compiling program $file")	
def logicalFile = resolver.getLogicalFile()

// create the appropriate parm list
def compileParms = "AG,GN,M,NEST,NIS,OP,SOURCE,X(S),DEFAULT(NONASGN),SYSTEM(CICS),ATTRIBUTES"
if (properties.errPrefix) {
    compileParms = "$compileParms,XINFO(XML)"
}

// define the MVSExec command to compile the file
def compile = new MVSExec().file(file).pgm("IBMZPLI").parm(compileParms)

// add DD statements to the MVSExec command
compile.dd(new DDStatement().name("SYSIN").dsn("$srcPDS($member)").options("shr").report(true))
compile.dd(new DDStatement().name("SYSLIN").dsn("$objectPDS($member)").options("shr").output(true))
compile.dd(new DDStatement().name("SYSPRINT").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT1").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSTERM").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSPUNCH").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSOUT").options(tempCreateOptions))

// add a syslib to the compile command with optional CICS concatenation
compile.dd(new DDStatement().name("SYSLIB").dsn(copyPDS).options("shr"))

// add a tasklib to the MVSExec command with optional CICS and IDz concatenation
compile.dd(new DDStatement().name("TASKLIB").dsn(properties.SIBMZCMP).options("shr"))
if (logicalFile.isCICS()) {
    compile.dd(new DDStatement().dsn(properties.SDFHLOAD).options("shr"))
}
if (properties.SFEKLOAD) {
    compile.dd(new DDStatement().dsn(properties.SFEKLOAD).options("shr"))
}

// add IDz User Build Error Feedback DDs
if (properties.errPrefix) {
    compile.dd(new DDStatement().name("SYSADATA").options("DUMMY"))
    compile.dd(new DDStatement().name("SYSXMLSD").dsn("${properties.hlq}.${properties.errPrefix}.SYSXMLSD.XML").options("mod keep"))
}

// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
compile.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(properties.logEncoding))

// execute the MVSExec compile command
def rc = compile.execute()

// update build result
tools.updateBuildResult(file:"$file", rc:rc, maxRC:4, log:logFile)
