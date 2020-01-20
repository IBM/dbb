@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*


// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def buildUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BuildUtilities.groovy"))
@Field def impactUtils= loadScript(new File("${props.zAppBuildDir}/utilities/ImpactUtilities.groovy"))
@Field def bindUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BindUtilities.groovy"))
@Field RepositoryClient repositoryClient

println("** Building files mapped to ${this.class.getName()}.groovy script")

// verify required build properties
buildUtils.assertBuildProperties(props.cobol_requiredBuildProperties)

// sort the build list based on build file rank if provided
List<String> sortedList = buildUtils.sortBuildList(argMap.buildList, 'cobol_fileBuildRank')

// iterate through build list
sortedList.each { buildFile ->
	println "*** Building file $buildFile"
	
	// copy build file and dependency files to data sets
	String rules = props.getFileProperty('cobol_resolutionRules', buildFile)
	DependencyResolver dependencyResolver = buildUtils.createDependencyResolver(buildFile, rules)
    buildUtils.copySourceFiles(buildFile, props.cobol_srcPDS, props.cobol_cpyPDS, dependencyResolver)
	
	// create mvs commands
	LogicalFile logicalFile = dependencyResolver.getLogicalFile()
	String member = CopyToPDS.createMemberName(buildFile)
	File logFile = new File( props.userBuild ? "${props.buildOutDir}/${member}.log" : "${props.buildOutDir}/${member}.cobol.log")
	if (logFile.exists())
		logFile.delete()
	MVSExec compile = createCompileCommand(buildFile, logicalFile, member, logFile)
	MVSExec linkEdit = createLinkEditCommand(buildFile, logicalFile, member, logFile)
	
	// execute mvs commands in a mvs job
	MVSJob job = new MVSJob()
	job.start()
	
	// compile the cobol program
	int rc = compile.execute()
	int maxRC = props.getFileProperty('cobol_compileMaxRC', buildFile).toInteger()
	
	boolean bindFlag = true
	
	if (rc > maxRC) {
		bindFlag = false
		String errorMsg = "*! The compile return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
		println(errorMsg)
		props.error = "true"
		buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
	}
	else {
	// if this program needs to be link edited . . .
		String needsLinking = props.getFileProperty('cobol_linkEdit', buildFile)
		if (needsLinking.toBoolean()) {
			rc = linkEdit.execute()
			maxRC = props.getFileProperty('cobol_linkEditMaxRC', buildFile).toInteger()
		
			if (rc > maxRC) {
				bindFlag = false
				String errorMsg = "*! The link edit return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
				println(errorMsg)
				props.error = "true"
				buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
			}
			else {
				if(!props.userBuild){
					// only scan the load module if load module scanning turned on for file
					String scanLoadModule = props.getFileProperty('cobol_scanLoadModule', buildFile)
					if (scanLoadModule && scanLoadModule.toBoolean() && getRepositoryClient())
						impactUtils.saveStaticLinkDependencies(buildFile, props.linkedit_loadPDS, logicalFile, repositoryClient)
				}
			}
	   	}	
	}
	
	//perform Db2 Bind only on User Build and perfromBindPackage property
	if (props.userBuild && bindFlag && logicalFile.isSQL() && props.bind_performBindPackage && props.bind_performBindPackage.toBoolean() ) {
		int bindMaxRC = props.getFileProperty('bind_maxRC', buildFile).toInteger()

		// if no  owner is set, use the user.name as package owner 
		def owner = ( !props.bind_packageOwner ) ? System.getProperty("user.name") : props.bind_packageOwner
	
		def (bindRc, bindLogFile) = bindUtils.bindPackage(buildFile, props.cobol_dbrmPDS, props.buildOutDir, props.bind_runIspfConfDir, 
				props.bind_db2Location, props.bind_collectionID, owner, props.bind_qualifier, props.verbose && props.verbose.toBoolean());
		if ( bindRc > bindMaxRC) {
			String errorMsg = "*! The bind package return code ($bindRc) for $buildFile exceeded the maximum return code allowed ($props.bind_maxRC)"
			println(errorMsg)
			props.error = "true"
			buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}_bind.log":bindLogFile],client:getRepositoryClient())
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
 * createCobolParms - Builds up the COBOL compiler parameter list from build and file properties
 */
def createCobolParms(String buildFile, LogicalFile logicalFile) {
	def parms = props.getFileProperty('cobol_compileParms', buildFile) ?: ""
	def cics = props.getFileProperty('cobol_compileCICSParms', buildFile) ?: ""
	def sql = props.getFileProperty('cobol_compileSQLParms', buildFile) ?: ""
	def errPrefixOptions = props.getFileProperty('cobol_compileErrorPrefixParms', buildFile) ?: ""
	def compileDebugParms = props.getFileProperty('cobol_compileDebugParms', buildFile)
	
	if (buildUtils.isCICS(logicalFile))
		parms = "$parms,$cics"
		
	if (buildUtils.isSQL(logicalFile))
		parms = "$parms,$sql"
	
	if (props.errPrefix)
		parms = "$parms,$errPrefixOptions"
		
	// add debug options
	if (props.debug)  {
		parms = "$parms,$compileDebugParms"
	}
		
	if (parms.startsWith(','))
		parms = parms.drop(1)
		
	if (props.verbose) println "Cobol compiler parms for $buildFile = $parms"
	return parms
}

/*
 * createCompileCommand - creates a MVSExec command for compiling the COBOL program (buildFile)
 */
def createCompileCommand(String buildFile, LogicalFile logicalFile, String member, File logFile) {
	String parms = createCobolParms(buildFile, logicalFile)
	String compiler = props.getFileProperty('cobol_compiler', buildFile)
	
	// define the MVSExec command to compile the program
	MVSExec compile = new MVSExec().file(buildFile).pgm(compiler).parm(parms)
	
	// add DD statements to the compile command
	compile.dd(new DDStatement().name("SYSIN").dsn("${props.cobol_srcPDS}($member)").options('shr').report(true))
	compile.dd(new DDStatement().name("SYSPRINT").options(props.cobol_printTempOptions))
	compile.dd(new DDStatement().name("SYSMDECK").options(props.cobol_tempOptions))
	(1..17).toList().each { num ->
		compile.dd(new DDStatement().name("SYSUT$num").options(props.cobol_tempOptions))
	}
	
	// Write SYSLIN to temporary dataset if performing link edit or to physical dataset
	String doLinkEdit = props.getFileProperty('cobol_linkEdit', buildFile)
	String linkEditStream = props.getFileProperty('cobol_linkEditStream', buildFile)
	String linkDebugExit = props.getFileProperty('cobol_linkDebugExit', buildFile)
	
	if ( (linkEditStream && doLinkEdit && doLinkEdit.toBoolean()) || (props.debug && linkDebugExit!= null && doLinkEdit)) {
		compile.dd(new DDStatement().name("SYSLIN").dsn("${props.cobol_objPDS}($member)").options('shr').output(true))
	}
	else {
		compile.dd(new DDStatement().name("SYSLIN").dsn("&&TEMPOBJ").options(props.cobol_tempOptions).pass(true))
	}
	
	// add a syslib to the compile command with optional bms output copybook and CICS concatenation
	compile.dd(new DDStatement().name("SYSLIB").dsn(props.cobol_cpyPDS).options("shr"))
	if (props.bms_cpyPDS)
		compile.dd(new DDStatement().dsn(props.bms_cpyPDS).options("shr"))
	if(props.team)
		compile.dd(new DDStatement().dsn(props.cobol_BMS_PDS).options("shr"))
	if (buildUtils.isCICS(logicalFile))
		compile.dd(new DDStatement().dsn(props.SDFHCOB).options("shr"))
	String isMQ = props.getFileProperty('cobol_isMQ', buildFile)
	if (isMQ && isMQ.toBoolean())
		compile.dd(new DDStatement().dsn(props.SCSQCOBC).options("shr"))

	// add a tasklib to the compile command with optional CICS, DB2, and IDz concatenations
	String compilerVer = props.getFileProperty('cobol_compilerVersion', buildFile)
	compile.dd(new DDStatement().name("TASKLIB").dsn(props."SIGYCOMP_$compilerVer").options("shr"))
	if (buildUtils.isCICS(logicalFile))
		compile.dd(new DDStatement().dsn(props.SDFHLOAD).options("shr"))
	if (buildUtils.isSQL(logicalFile))
		compile.dd(new DDStatement().dsn(props.SDSNLOAD).options("shr"))
	if (props.SFELLOAD)
		compile.dd(new DDStatement().dsn(props.SFELLOAD).options("shr"))
		
	// add optional DBRMLIB if build file contains DB2 code
	if (buildUtils.isSQL(logicalFile))
		compile.dd(new DDStatement().name("DBRMLIB").dsn("$props.cobol_dbrmPDS($member)").options('shr').output(true).deployType('DBRM'))

	// add IDz User Build Error Feedback DDs
	if (props.errPrefix) {
		compile.dd(new DDStatement().name("SYSADATA").options("DUMMY"))
		// SYSXMLSD.XML suffix is mandatory for IDZ/ZOD to populate remote error list
		compile.dd(new DDStatement().name("SYSXMLSD").dsn("${props.hlq}.${props.errPrefix}.SYSXMLSD.XML").options(props.cobol_compileErrorFeedbackXmlOptions))
	}
		
	// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
	compile.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding))
	
	return compile
}


/*
 * createLinkEditCommand - creates a MVSExec xommand for link editing the COBOL object module produced by the compile
 */
def createLinkEditCommand(String buildFile, LogicalFile logicalFile, String member, File logFile) {
	String parms = props.getFileProperty('cobol_linkEditParms', buildFile)
	String linker = props.getFileProperty('cobol_linkEditor', buildFile)
	String linkEditStream = props.getFileProperty('cobol_linkEditStream', buildFile)
	String linkDebugExit = props.getFileProperty('cobol_linkDebugExit', buildFile)
	
	// define the MVSExec command to link edit the program
	MVSExec linkedit = new MVSExec().file(buildFile).pgm(linker).parm(parms)
	
	// Create a pysical link card
	if ( (linkEditStream) || (props.debug && linkDebugExit!= null)) {
		def lnkFile = new File("${props.buildOutDir}/linkCard.lnk")
		if (lnkFile.exists())
			lnkFile.delete()

		if 	(linkEditStream)
			lnkFile << "  " + linkEditStream.replace("\\n","\n").replace('@{member}',member)
		else
			lnkFile << "  " + linkDebugExit.replace("\\n","\n").replace('@{member}',member)

		if (props.verbose)
			println("Copying ${props.buildOutDir}/linkCard.lnk to ${props.linkedit_srcPDS}($member)")
		new CopyToPDS().file(lnkFile).dataset(props.linkedit_srcPDS).member(member).execute()
		// Alloc SYSLIN
		linkedit.dd(new DDStatement().name("SYSLIN").dsn("${props.linkedit_srcPDS}($member)").options("shr"))
		// add the obj DD
		linkedit.dd(new DDStatement().name("OBJECT").dsn("${props.cobol_objPDS}($member)").options('shr'))

		//	} else if (props.debug && linkDebugExit!= null){
		//		//instream SYSLIN, requires DDName list
		//		String records = "  " + linkDebugExit.replace("\\n","\n").replace('@{member}',member)
		//		linkedit.dd(new DDStatement().name("SYSLIN").instreamData(records))
	} else { // no debug && no link card
		// Use &&TEMP from Compile
	}
	
	// add DD statements to the linkedit command
	String linkedit_deployType = props.getFileProperty('linkedit_deployType', buildFile)
	if ( linkedit_deployType == null )
		linkedit_deployType = 'LOAD'
	linkedit.dd(new DDStatement().name("SYSLMOD").dsn("${props.cobol_loadPDS}($member)").options('shr').output(true).deployType(linkedit_deployType))
	linkedit.dd(new DDStatement().name("SYSPRINT").options(props.cobol_printTempOptions))
	linkedit.dd(new DDStatement().name("SYSUT1").options(props.cobol_tempOptions))
	
	// add RESLIB if needed
	if ( props.RESLIB ) {
		linkedit.dd(new DDStatement().name("RESLIB").dsn(props.RESLIB).options("shr"))
	}
	
	// add a syslib to the compile command with optional CICS concatenation
	linkedit.dd(new DDStatement().name("SYSLIB").dsn(props.cobol_objPDS).options("shr"))
	linkedit.dd(new DDStatement().dsn(props.SCEELKED).options("shr"))
	
	// Add Debug Dataset to find the debug exit to SYSLIB
	if (props.debug && props.SEQAMOD)
		linkedit.dd(new DDStatement().dsn(props.SEQAMOD).options("shr"))
	
    if (buildUtils.isCICS(logicalFile))
		linkedit.dd(new DDStatement().dsn(props.SDFHLOAD).options("shr"))
		
	 String isMQ = props.getFileProperty('cobol_isMQ', buildFile)
     if (isMQ && isMQ.toBoolean())
    	linkedit.dd(new DDStatement().dsn(props.SCSQLOAD).options("shr"))

	// add a copy command to the linkedit command to append the SYSPRINT from the temporary dataset to the HFS log file
	linkedit.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))
	
	return linkedit
}


def getRepositoryClient() {
	if (!repositoryClient && props."dbb.RepositoryClient.url")
		repositoryClient = new RepositoryClient().forceSSLTrusted(true)
	
	return repositoryClient
}









