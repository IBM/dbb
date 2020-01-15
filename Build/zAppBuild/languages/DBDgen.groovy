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
buildUtils.assertBuildProperties(props.dbdgen_requiredBuildProperties)

// sort the build list based on build file rank if provided
List<String> sortedList = buildUtils.sortBuildList(argMap.buildList, 'dbdgen_fileBuildRank')

// iterate through build list
sortedList.each { buildFile ->
	println "*** Building file $buildFile"

	// copy build file to input data set
	buildUtils.copySourceFiles(buildFile, props.dbdgen_srcPDS, null, null)

	// create mvs commands
	String member = CopyToPDS.createMemberName(buildFile)
	File logFile = new File( props.userBuild ? "${props.buildOutDir}/${member}.log" : "${props.buildOutDir}/${member}.dbd.log")
	if (logFile.exists())
		logFile.delete()
	MVSExec assembler = createDBDAssemblyCommand(buildFile, member, logFile)
	MVSExec linkEdit = createDBDLinkEditCommand(buildFile, member, logFile)

	// execute mvs commands in a mvs job
	MVSJob job = new MVSJob()
	job.start()

	// asm the psb
	int rc = assembler.execute()
	int maxRC = props.getFileProperty('dbdgen_assemblerMaxRC', buildFile).toInteger()


	if (rc > maxRC) {
		String errorMsg = "*! The assembly return code for DBDgen ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
		println(errorMsg)
		props.error = "true"
		buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
	}
	else {

		rc = linkEdit.execute()
		maxRC = props.getFileProperty('dbdgen_linkEditMaxRC', buildFile).toInteger()

		if (rc > maxRC) {
			String errorMsg = "*! The link edit return code for DBDgen ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
			println(errorMsg)
			props.error = "true"
			buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
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
 * createPhase1Command - creates a MVSExec command for preprocessing the MFS Map (buildFile)
 */
def createDBDAssemblyCommand(String buildFile, String member, File logFile) {

	//         PROC MBR=TEMPNAME,SOUT='*',RGN=0M,VER=V13R1,PLEX=IMSC        00010009
	//C        EXEC PGM=ASMA90,REGION=&RGN,                                 00020000
	//             PARM=(OBJECT,NODECK,NODBCS,'SIZE(MAX,ABOVE)')            00030004
	//SYSLIB    DD DSN=IMS.&VER..SDFSMAC,DISP=SHR                           00050000
	//SYSIN     DD DISP=SHR,DSN=IMSCFG.&PLEX..DBD(&MBR)                     00051003
	//SYSLIN    DD UNIT=SYSDA,DISP=(,PASS),                                 00060000
	//             SPACE=(TRK,(1,1),RLSE),BLKSIZE=80,RECFM=FB               00080000
	//SYSPRINT  DD SYSOUT=&SOUT,SPACE=(TRK,(1,1),RLSE)                      00100000
	//SYSUT1    DD UNIT=SYSDA,SPACE=(CYL,(10,5))                            00120000
	//         IF C.RC = 0 THEN                                             00121000
	def String parameters = props.getFileProperty('dbdgen_pgmParms', buildFile)
	def String errPrefixOptions = props.getFileProperty('dbdgen_compileErrorPrefixParms', buildFile) ?: ""
	
	if (props.errPrefix)
		parameters = "$parameters,$errPrefixOptions"

	MVSExec assembler = new MVSExec().file(buildFile).pgm(props.dbdgen_pgm).parm(parameters)

	// add DD statements to the compile command
	String assembler_srcPDS = props.getFileProperty('dbdgen_srcPDS', buildFile)

	assembler.dd(new DDStatement().name("SYSIN").dsn("${assembler_srcPDS}($member)").options('shr').report(true))
	assembler.dd(new DDStatement().name("SYSPRINT").options(props.dbdgen_tempOptions))
	assembler.dd(new DDStatement().name("SYSUT1").options(props.dbdgen_tempOptions))

	// Write SYSLIN to temporary dataset if performing link edit
	assembler.dd(new DDStatement().name("SYSLIN").dsn("&&TEMPOBJ").options(props.dbdgen_tempOptions).pass(true))

	// create a SYSLIB concatenation with optional MACLIB and MODGEN
	assembler.dd(new DDStatement().name("SYSLIB").dsn(props.SDFSMAC).options("shr"))
	if (props.SCEEMAC)
		assembler.dd(new DDStatement().dsn(props.SCEEMAC).options("shr"))

	// add IDz User Build Error Feedback DDs
	if (props.errPrefix) {
		assembler.dd(new DDStatement().name("SYSADATA").options("DUMMY"))
		// SYSXMLSD.XML suffix is mandatory for IDZ/ZOD to populate remote error list
		assembler.dd(new DDStatement().name("SYSXMLSD").dsn("${props.hlq}.${props.errPrefix}.SYSXMLSD.XML").options(props.dbdgen_compileErrorFeedbackXmlOptions))
	}
		
	// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
	assembler.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))

	return assembler
}

/*
 * createPhase1Command - creates a MVSExec command for preprocessing the MFS Map (buildFile)
 */
def createDBDLinkEditCommand(String buildFile, String member, File logFile) {

	//L        EXEC PGM=IEWL,PARM='XREF,LIST',REGION=4M                     00130000
	//SYSLIN    DD DSN=*.C.SYSLIN,DISP=(OLD,DELETE)                         00150000
	//SYSPRINT  DD SYSOUT=&SOUT,SPACE=(TRK,(1,1),RLSE)                      00160000
	//SYSLMOD   DD DISP=SHR,DSN=IMSCFG.&PLEX..DBDLIB(&MBR)                  00180001
	//SYSUT1    DD UNIT=(SYSDA,SEP=(SYSLMOD,SYSLIN)),SPACE=(TRK,(1,1))      00200000
	//         ENDIF                                                        00220000                                 
	
	String parameters = props.getFileProperty('dbdgen_linkEditParms', buildFile)

	// define the MVSExec command to link edit the program
	MVSExec linkedit = new MVSExec().file(buildFile).pgm(props.dbdgen_linkEditor).parm(parameters)

	// add DD statements to the linkedit command
	String dbdgen_loadPDS = props.getFileProperty('dbdgen_loadPDS', buildFile)
	String dbdgen_deployType = props.getFileProperty('dbdgen_deployType', buildFile)
	if ( dbdgen_deployType == null )
		dbdgen_deployType = 'LOAD'
	linkedit.dd(new DDStatement().name("SYSLMOD").dsn("${dbdgen_loadPDS}($member)").options('shr').output(true).deployType(dbdgen_deployType))
	linkedit.dd(new DDStatement().name("SYSPRINT").options(props.dbdgen_tempOptions))
	linkedit.dd(new DDStatement().name("SYSUT1").options(props.dbdgen_tempOptions))

	// add a copy command to the linkedit command to append the SYSPRINT from the temporary dataset to the HFS log file
	linkedit.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))

	return linkedit
}


def getRepositoryClient() {
	if (!repositoryClient && props."dbb.RepositoryClient.url")
		repositoryClient = new RepositoryClient().forceSSLTrusted(true)

	return repositoryClient
}



