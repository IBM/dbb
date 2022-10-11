import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*

// receive passed arguments
def file = args[0]
println("* Building $file using ${this.class.getName()}.groovy script")

// define local properties
def properties = BuildProperties.getInstance()
def srcPDS = "${properties.hlq}.COBOLLIB"
def copybookPDS = "${properties.hlq}.COPYBOOK"
def objectPDS = "${properties.hlq}.COBOL"
def member = CopyToPDS.createMemberName(file)
def logFile = new File("${properties.workDir}/${member}.log")

// create a reference to the Tools.groovy utility script
File scriptFile = new File("$properties.sourceDir/Build/IDE/build/Tools.groovy")
Class groovyClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(scriptFile)
GroovyObject tools = (GroovyObject) groovyClass.newInstance()

// define the BPXWDYN options for allocated temporary datasets
def tempCreateOptions = "cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"

// copy program to PDS
println("Copying ${properties.sourceDir}/$file to $srcPDS($member)")
new CopyToPDS().file(new File("${properties.sourceDir}/$file")).dataset(srcPDS).member(member).execute()

//resolve program dependencies and copy to PDS
println("Resolving dependencies for file $file")
def resolver = tools.getCOBDependencyResolver(file)
def deps = resolver.resolve()
new CopyToPDS().dependencies(deps).dataset(copybookPDS).execute()

// compile the build file
println("Compiling build file $file")
def logicalFile = resolver.getLogicalFile()

// create the appropriate parm list
def parms = "APOST,DATA(31),LIB,NODYNAM,OBJ,OPT,RENT,RES"
if (properties.errPrefix) {
    parms = "$parms,ADATA,EX(ADX(ELAXMGUX))"
}

// define the MVSExec command to compile the program
def compile = new MVSExec().file(file).pgm("IGYCRCTL").parm(parms)

// add DD statements to the MVSExec command
compile.dd(new DDStatement().name("SYSIN").dsn("$srcPDS($member)").options("shr").report(true))
compile.dd(new DDStatement().name("SYSLIN").dsn("$objectPDS($member)").options("shr").output(true))
compile.dd(new DDStatement().name("SYSPRINT").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSADATA").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT1").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT2").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT3").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT4").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT5").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT6").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT7").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT8").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT9").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT10").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT11").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT12").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT13").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT14").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT15").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT16").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSUT17").options(tempCreateOptions))
compile.dd(new DDStatement().name("SYSMDECK").options(tempCreateOptions))

// add a syslib to the MVSExec command with optional CICS concatenation
compile.dd(new DDStatement().name("SYSLIB").dsn(copybookPDS).options("shr"))
//if (properties.team) {
       // for user builds concatenate the team build copbook pds
//       compile.dd(new DDStatement().dsn("${properties.team}.COPYBOOK").options("shr"))
//}

// add a tasklib to the MVSExec command with optional CICS and IDz concatenation
compile.dd(new DDStatement().name("TASKLIB").dsn(properties.SIGYCOMP).options("shr"))
if (properties.SFEKLOAD) {
    compile.dd(new DDStatement().dsn(properties.SFEKLOAD).options("shr"))
}

// add IDz User Build Error Feedback DDs
if (properties.errPrefix) {
    compile.dd(new DDStatement().name("SYSXMLSD").dsn("${properties.hlq}.${properties.errPrefix}.SYSXMLSD.XML").options("mod keep"))
}


// add a copy command to the MVSExec command to copy the SYSPRINT from the temporary dataset to an HFS log file
compile.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(properties.logEncoding))

// execute the MVSExec compile command
def rc = compile.execute()

// update build result
tools.updateBuildResult(file:"$file", rc:rc, maxRC:4, log:logFile)
	
