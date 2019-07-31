@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*


// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def buildUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BuildUtilities.groovy"))

@Field RepositoryClient repositoryClient

println("** Building files mapped to ${this.class.getName()}.groovy script")

// verify required build properties
buildUtils.assertBuildProperties(props.bms_requiredBuildProperties)

// sort the build list based on build file rank if provided
List<String> sortedList = buildUtils.sortBuildList(argMap.buildList, 'bms_fileBuildRank')

// iterate through build list
sortedList.each { buildFile ->
	println "*** Building file $buildFile"
	
	// copy build file to input data set
	buildUtils.copySourceFiles(buildFile, props.bms_srcPDS, null, null)
	
	// create mvs commands
	String member = CopyToPDS.createMemberName(buildFile)
	File logFile = new File( props.userBuild ? "${props.buildOutDir}/${member}.log" : "${props.buildOutDir}/${member}.bms.log")
	if (logFile.exists())
		logFile.delete()
	MVSExec copyGen = createCopyGenCommand(buildFile, member, logFile)
	MVSExec compile = createCompileCommand(buildFile, member, logFile)
	MVSExec linkEdit = createLinkEditCommand(buildFile, member, logFile)
	
	// execute mvs commands in a mvs job
	def maxRC = props.getFileProperty('bms_maxRC', buildFile).toInteger()
        def rc = new MVSJob().executable(copyGen)
	                     .executable(compile)
			     .executable(linkEdit)
			     .maxRC(0)
			     .execute()
	
    if (rc > maxRC) {
	    String errorMsg = "*! The build return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
		println(errorMsg)
		props.error = "true"
		buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
	 }
	 
}

// end script


//********************************************************************
//* Method definitions
//********************************************************************

/*
 * createCompileCommand - creates a MVSExec command for compiling the BMS Map (buildFile)
 */
def createCopyGenCommand(String buildFile, String member, File logFile) {
	String parameters = props.getFileProperty('bms_copyGenParms', buildFile)
	
	// define the MVSExec command to compile the BMS map
	MVSExec compile = new MVSExec().file(buildFile).pgm(props.bms_assembler).parm(parameters)
	
	// add DD statements to the compile command
	compile.dd(new DDStatement().name("SYSIN").dsn("${props.bms_srcPDS}($member)").options('shr').report(true))
	compile.dd(new DDStatement().name("SYSPRINT").options(props.bms_tempOptions))
	compile.dd(new DDStatement().name("SYSPUNCH").dsn("${props.bms_cpyPDS}($member)").options('shr').output(true))
	[1,2,3].each { num ->
		compile.dd(new DDStatement().name("SYSUT$num").options(props.bms_tempOptions))
	}
	compile.dd(new DDStatement().name("SYSLIB").dsn(props.SDFHMAC).options("shr"))
	compile.dd(new DDStatement().dsn(props.MACLIB).options("shr"))
	compile.dd(new DDStatement().name("TASKLIB").dsn(props.SASMMOD1).options("shr"))
		
	// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
	compile.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding))
	
	return compile
}

/*
 * createCompileCommand - creates a MVSExec command for compiling the BMS Map (buildFile)
 */
def createCompileCommand(String buildFile, String member, File logFile) {
	String parameters = props.getFileProperty('bms_compileParms', buildFile)
	
	// define the MVSExec command to compile the BMS map
	MVSExec compile = new MVSExec().file(buildFile).pgm(props.bms_assembler).parm(parameters)
	
	// add DD statements to the compile command
	compile.dd(new DDStatement().name("SYSIN").dsn("${props.bms_srcPDS}($member)").options('shr'))
	compile.dd(new DDStatement().name("SYSPRINT").options(props.bms_tempOptions))
	compile.dd(new DDStatement().name("SYSPUNCH").dsn("&&TEMPOBJ").options(props.bms_tempOptions).pass(true))
	[1,2,3].each { num ->
		compile.dd(new DDStatement().name("SYSUT$num").options(props.bms_tempOptions))
	}
	compile.dd(new DDStatement().name("SYSLIB").dsn(props.SDFHMAC).options("shr"))
	compile.dd(new DDStatement().dsn(props.MACLIB).options("shr"))
	compile.dd(new DDStatement().name("TASKLIB").dsn(props.SASMMOD1).options("shr"))
		
	compile.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))
	
	return compile
}


/*
 * createLinkEditCommand - creates a MVSExec xommand for link editing the bms object module produced by the compile
 */
def createLinkEditCommand(String buildFile, String member, File logFile) {
	String parameters = props.getFileProperty('bms_linkEditParms', buildFile)
	
	// define the MVSExec command to link edit the program
	MVSExec linkedit = new MVSExec().file(buildFile).pgm(props.bms_linkEditor).parm(parameters)
	
	// add DD statements to the linkedit command
	linkedit.dd(new DDStatement().name("SYSLIN").dsn("&&TEMPOBJ").options("shr"))
	linkedit.dd(new DDStatement().name("SYSLMOD").dsn("${props.bms_loadPDS}($member)").options('shr').output(true).deployType('MAPLOAD'))
	linkedit.dd(new DDStatement().name("SYSPRINT").options(props.bms_tempOptions))
	linkedit.dd(new DDStatement().name("SYSUT1").options(props.bms_tempOptions))
	
	// add a syslib to the linkedit command
	linkedit.dd(new DDStatement().name("SYSLIB").dsn(props.SCEELKED).options("shr"))
	linkedit.dd(new DDStatement().dsn(props.SDFHLOAD).options("shr"))

	// add a copy command to the linkedit command to append the SYSPRINT from the temporary dataset to the HFS log file
	linkedit.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding))
	
	return linkedit
}

def getRepositoryClient() {
	if (!repositoryClient && props."dbb.RepositoryClient.url")
		repositoryClient = new RepositoryClient().forceSSLTrusted(true)
	
	return repositoryClient
}



