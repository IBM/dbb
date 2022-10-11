import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*

// receive passed arguments
def file = args[0]
println("* Building $file using ${this.class.getName()}.groovy script")

// define local properties
def properties = BuildProperties.getInstance()
def srcPDS = "${properties.hlq}.ASMLIB"
def macroPDS = "${properties.hlq}.MACRO"
def objectPDS = "${properties.hlq}.ASM"
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

//resolve program dependencies
println("Resolving dependencies for file $file")
def resolver = tools.getASMDependencyResolver(file)
def deps = resolver.resolve()
new CopyToPDS().dependencies(deps).dataset(macroPDS).execute()

// compile the build file
println("Compiling program $file")
def logicalFile = resolver.getLogicalFile()

// define the MVSExec command to compile the file
def compile = new MVSExec().file(file).pgm("ASMA90").parm("")

// add DD statements to the MVSExec command
compile.dd(new DDStatement().name("SYSIN").dsn("$srcPDS($member)").options("shr").report(true))
compile.dd(new DDStatement().name("SYSLIN").dsn("$objectPDS($member)").options("shr").output(true))
compile.dd(new DDStatement().name("SYSPRINT").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT1").options(tempCreateOptions))

// add a syslib to the compile command with optional CICS concatenation
compile.dd(new DDStatement().name("SYSLIB").dsn(macroPDS).options("shr"))
if (properties.MACLIB) {
	compile.dd(new DDStatement().dsn(properties.MACLIB).options("shr"))
}
if (properties.MODGEN) {
	compile.dd(new DDStatement().dsn(properties.MODGEN).options("shr"))
}

// add IDz User Build Error Feedback DDs
if (properties.errPrefix) {
    compile.dd(new DDStatement().name("SYSADATA").options("DUMMY"))
    compile.dd(new DDStatement().name("SYSXMLSD").dsn("${properties.hlq}.${properties.errPrefix}.SYSXMLSD.XML").options("mod keep"))
}

// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
compile.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding("IBM-037"))

// execute the MVSExec compile command
def rc = compile.execute()

// update build result
tools.updateBuildResult(file:"$file", rc:rc, maxRC:4, log:logFile)
