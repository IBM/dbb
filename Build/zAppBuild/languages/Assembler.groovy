@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*


// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def buildUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BuildUtilities.groovy"))
@Field def impactUtils= loadScript(new File("${props.zAppBuildDir}/utilities/ImpactUtilities.groovy"))
@Field RepositoryClient repositoryClient

println("** Building files mapped to ${this.class.getName()}.groovy script")

// verify required build properties
buildUtils.assertBuildProperties(props.assembler_requiredBuildProperties)

// sort the build list based on build file rank if provided
List<String> sortedList = buildUtils.sortBuildList(argMap.buildList, 'assembler_fileBuildRank')

// iterate through build list
sortedList.each { buildFile ->
	println "*** Building file $buildFile"
	
	// copy build file to input data set
	// copy build file and dependency files to data sets
	String rules = props.getFileProperty('assembler_resolutionRules', buildFile)
	String assembler_srcPDS = props.getFileProperty('assembler_srcPDS', buildFile)
	DependencyResolver dependencyResolver = buildUtils.createDependencyResolver(buildFile, rules)
	buildUtils.copySourceFiles(buildFile, assembler_srcPDS, props.assembler_macroPDS, dependencyResolver)	
	
	// create mvs commands
	LogicalFile logicalFile = dependencyResolver.getLogicalFile()
	String member = CopyToPDS.createMemberName(buildFile)
	File logFile = new File( props.userBuild ? "${props.buildOutDir}/${member}.log" : "${props.buildOutDir}/${member}.asm.log")
	if (logFile.exists())
		logFile.delete()
	MVSExec assembler = createAssemblerCommand(buildFile, member, logFile)
	MVSExec linkEdit = createLinkEditCommand(buildFile, member, logFile)
	
	// execute mvs commands in a mvs job
	MVSJob job = new MVSJob()
	job.start()
	
	// assemble the program
	int rc = assembler.execute()
	int maxRC = props.getFileProperty('assembler_maxRC', buildFile).toInteger()
	
	if (rc > maxRC) {
		String errorMsg = "*! The assembler return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
		println(errorMsg)
		props.error = "true"
		buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
	}
	else {
	// if this program needs to be link edited . . .
		String needsLinking = props.getFileProperty('assembler_linkEdit', buildFile)
		if (needsLinking && needsLinking.toBoolean()) {
			rc = linkEdit.execute()
			maxRC = props.getFileProperty('assembler_linkEditMaxRC', buildFile).toInteger()
		
			if (rc > maxRC) {
				String errorMsg = "*! The link edit return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
				println(errorMsg)
				props.error = "true"
				buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
			}
			else {
				// only scan the load module if load module scanning turned on for file
				String scanLoadModule = props.getFileProperty('assembler_scanLoadModule', buildFile)
				if (scanLoadModule && scanLoadModule.toBoolean() && getRepositoryClient()) {
					String assembler_loadPDS = props.getFileProperty('assembler_loadPDS', buildFile)
					impactUtils.saveStaticLinkDependencies(buildFile, assembler_loadPDS, logicalFile, repositoryClient)
				}
			}
		}
			
	}
	
	
	// clean up passed DD statements
	job.stop()
	
}

// end script

//********************************************************************
 //* Method definitions
 //********************************************************************

/*
 * createCompileCommand - creates a MVSExec command for compiling the BMS Map (buildFile)
 */
def createAssemblerCommand(String buildFile, String member, File logFile) {
	def errPrefixOptions = props.getFileProperty('assembler_compileErrorPrefixParms', buildFile) ?: ""
	
	String parameters = props.getFileProperty('assembler_pgmParms', buildFile)
	
	if (props.errPrefix)
		parameters = "$parameters,$errPrefixOptions"
	
	// define the MVSExec command to compile the BMS map
	MVSExec assembler = new MVSExec().file(buildFile).pgm(props.assembler_pgm).parm(parameters)
	
	// add DD statements to the compile command
	String assembler_srcPDS = props.getFileProperty('assembler_srcPDS', buildFile)
	assembler.dd(new DDStatement().name("SYSIN").dsn("${assembler_srcPDS}($member)").options('shr'))
	assembler.dd(new DDStatement().name("SYSPRINT").options(props.assembler_tempOptions))
	assembler.dd(new DDStatement().name("SYSUT1").options(props.assembler_tempOptions))

	
	// Write SYSLIN to temporary dataset if performing link edit
	String doLinkEdit = props.getFileProperty('assembler_linkEdit', buildFile)
	if (doLinkEdit && doLinkEdit.toBoolean())
		assembler.dd(new DDStatement().name("SYSLIN").dsn("&&TEMPOBJ").options(props.assembler_tempOptions).pass(true))
	else
		assembler.dd(new DDStatement().name("SYSLIN").dsn("${props.assembler_objPDS}($member)").options('shr').output(true))
	
	// create a SYSLIB concatenation with optional MACLIB and MODGEN	
	assembler.dd(new DDStatement().name("SYSLIB").dsn(props.assembler_macroPDS).options("shr"))
	if (props.SCEEMAC)
		assembler.dd(new DDStatement().dsn(props.SCEEMAC).options("shr"))
	if (props.MACLIB)
		assembler.dd(new DDStatement().dsn(props.MACLIB).options("shr"))
	if (props.MODGEN)
		assembler.dd(new DDStatement().dsn(props.MODGEN).options("shr"))
	if (props.SDFSMAC)
		assembler.dd(new DDStatement().dsn(props.SDFSMAC).options("shr"))
		
	// add IDz User Build Error Feedback DDs
	if (props.errPrefix) {
		assembler.dd(new DDStatement().name("SYSADATA").options("DUMMY"))
		new CreatePDS().dataset("${props.hlq}.${props.errPrefix}.SYSXMLSD.XML").options(props.assembler_compileErrorFeedbackXmlOptions).create()
		assembler.dd(new DDStatement().name("SYSXMLSD").dsn("${props.hlq}.${props.errPrefix}.SYSXMLSD.XML").options("mod keep"))
	}
		
	// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
	assembler.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))
	
	return assembler
}


/*
 * createLinkEditCommand - creates a MVSExec xommand for link editing the assembler object module produced by the compile
 */
def createLinkEditCommand(String buildFile, String member, File logFile) {
	String parameters = props.getFileProperty('assembler_linkEditParms', buildFile)
	
	// define the MVSExec command to link edit the program
	MVSExec linkedit = new MVSExec().file(buildFile).pgm(props.assembler_linkEditor).parm(parameters)
	
	// add DD statements to the linkedit command
	String assembler_loadPDS = props.getFileProperty('assembler_loadPDS', buildFile)
	String assembler_deployType = props.getFileProperty('assembler_deployType', buildFile)
	if ( assembler_deployType == null )
		assembler_deployType = 'LOAD'
	linkedit.dd(new DDStatement().name("SYSLMOD").dsn("${assembler_loadPDS}($member)").options('shr').output(true).deployType(assembler_deployType))
	linkedit.dd(new DDStatement().name("SYSPRINT").options(props.assembler_tempOptions))
	linkedit.dd(new DDStatement().name("SYSUT1").options(props.assembler_tempOptions))
	
	// add a syslib to the linkedit command
	linkedit.dd(new DDStatement().name("SYSLIB").dsn(props.assembler_objPDS).options("shr"))
	linkedit.dd(new DDStatement().dsn(props.SCEELKED).options("shr"))
	if ( props.SDFHLOAD )
		linkedit.dd(new DDStatement().dsn(props.SDFHLOAD).options("shr"))

	// add a copy command to the linkedit command to append the SYSPRINT from the temporary dataset to the HFS log file
	linkedit.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))
	
	return linkedit
}

def getRepositoryClient() {
	if (!repositoryClient && props."dbb.RepositoryClient.url")
		repositoryClient = new RepositoryClient().forceSSLTrusted(true)
	
	return repositoryClient
}


