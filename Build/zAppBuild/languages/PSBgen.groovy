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
buildUtils.assertBuildProperties(props.psbgen_requiredBuildProperties)

// sort the build list based on build file rank if provided
List<String> sortedList = buildUtils.sortBuildList(argMap.buildList, 'psbgen_fileBuildRank')

// iterate through build list
sortedList.each { buildFile ->
	println "*** Building file $buildFile"

	// copy build file to input data set
	buildUtils.copySourceFiles(buildFile, props.psbgen_srcPDS, null, null)

	// create mvs commands
	String member = CopyToPDS.createMemberName(buildFile)
	File logFile = new File( props.userBuild ? "${props.buildOutDir}/${member}.log" : "${props.buildOutDir}/${member}.psb.log")
	if (logFile.exists())
		logFile.delete()
	MVSExec assembler = createPSBAssemblyCommand(buildFile, member, logFile)
	MVSExec linkEdit = createPSBLinkEditCommand(buildFile, member, logFile)
	MVSExec acbgenCommand = createACBgenCommand(buildFile, member, logFile)

	// execute mvs commands in a mvs job
	MVSJob job = new MVSJob()
	job.start()

	// asm the psb
	int rc = assembler.execute()
	int maxRC = props.getFileProperty('psbgen_assemblerMaxRC', buildFile).toInteger()


	if (rc > maxRC) {
		String errorMsg = "*! The assembly return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
		println(errorMsg)
		props.error = "true"
		buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
	}
	else {

		rc = linkEdit.execute()
		maxRC = props.getFileProperty('psbgen_linkEditMaxRC', buildFile).toInteger()

		if (rc > maxRC) {
			String errorMsg = "*! The link edit return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
			println(errorMsg)
			props.error = "true"
			buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
		}

		else{
			if (props.psbgen_runACBgen){ // only run acbgen, if set to true in PSBgen.properties
				rc = acbgenCommand.execute()
				maxRC = props.getFileProperty('acbgen_pgmMaxRC', buildFile).toInteger()

				if (rc > maxRC) {
					String errorMsg = "*! The acbgen return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
					println(errorMsg)
					props.error = "true"
					buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
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
 * createPhase1Command - creates a MVSExec command for preprocessing the MFS Map (buildFile)
 */
def createPSBAssemblyCommand(String buildFile, String member, File logFile) {

	// SAMPLE JCL
	//         PROC MBR=TEMPNAME,SOUT='*',RGN=0M,VER=V13R1,PLEX=IMSC        00010011
	//C        EXEC PGM=ASMA90,REGION=&RGN,                                 00020000
	//             PARM=(OBJECT,NODECK,NODBCS,'SIZE(MAX,ABOVE)')            00030004
	//SYSLIB    DD DSN=IMS.&VER..SDFSMAC,DISP=SHR                           00050000
	//SYSIN     DD DISP=SHR,DSN=IMSCFG.&PLEX..PSB(&MBR)                     00051003
	//SYSLIN    DD UNIT=SYSDA,DISP=(,PASS),                                 00060000
	//             SPACE=(TRK,(1,1),RLSE),BLKSIZE=80,RECFM=FB               00080000
	//SYSPRINT  DD SYSOUT=&SOUT,SPACE=(TRK,(1,1),RLSE)                      00100000
	//SYSUT1    DD UNIT=SYSDA,SPACE=(CYL,(10,5))                            00120000

	def String parameters = props.getFileProperty('psbgen_pgmParms', buildFile)
	def String errPrefixOptions = props.getFileProperty('psbgen_compileErrorPrefixParms', buildFile) ?: ""
		
	if (props.errPrefix)
		parameters = "$parameters,$errPrefixOptions"
	
	MVSExec assembler = new MVSExec().file(buildFile).pgm(props.psbgen_pgm).parm(parameters)

	// add DD statements to the compile command
	String assembler_srcPDS = props.getFileProperty('psbgen_srcPDS', buildFile)

	assembler.dd(new DDStatement().name("SYSIN").dsn("${assembler_srcPDS}($member)").options('shr').report(true))
	assembler.dd(new DDStatement().name("SYSPRINT").options(props.psbgen_tempOptions))
	assembler.dd(new DDStatement().name("SYSUT1").options(props.psbgen_tempOptions))

	// Write SYSLIN to temporary dataset if performing link edit
	assembler.dd(new DDStatement().name("SYSLIN").dsn("&&TEMPOBJ").options(props.psbgen_tempOptions).pass(true))

	// create a SYSLIB concatenation with optional MACLIB and MODGEN
	assembler.dd(new DDStatement().name("SYSLIB").dsn(props.SDFSMAC).options("shr"))
	if (props.SCEEMAC)
		assembler.dd(new DDStatement().dsn(props.SCEEMAC).options("shr"))

	// add IDz User Build Error Feedback DDs
	if (props.errPrefix) {
		assembler.dd(new DDStatement().name("SYSADATA").options("DUMMY"))
		// SYSXMLSD.XML suffix is mandatory for IDZ/ZOD to populate remote error list
		assembler.dd(new DDStatement().name("SYSXMLSD").dsn("${props.hlq}.${props.errPrefix}.SYSXMLSD.XML").options(props.psbgen_compileErrorFeedbackXmlOptions))
	}
		
		
	// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
	assembler.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))

	return assembler
}

/*
 * createPhase2Command - creates a MVSExec command for preprocessing the MFS Map (buildFile)
 */
def createPSBLinkEditCommand(String buildFile, String member, File logFile) {

	//         PROC MBR=TEMPNAME,SOUT='*',RGN=0M,VER=V13R1,PLEX=IMSC        00010011

	//L        EXEC PGM=IEWL,PARM='XREF,LIST',REGION=4M                     00130000
	//SYSLIN    DD DSN=*.C.SYSLIN,DISP=(OLD,DELETE)                         00150000
	//SYSPRINT  DD SYSOUT=&SOUT,SPACE=(TRK,(1,1),RLSE)                      00160000
	//SYSLMOD   DD DISP=SHR,DSN=IMSCFG.&PLEX..PSBLIB(&MBR)                  00180001
	//SYSUT1    DD UNIT=(SYSDA,SEP=(SYSLMOD,SYSLIN)),SPACE=(TRK,(1,1))      00200000
	//         ENDIF                                                        00220000

	String parameters = props.getFileProperty('psbgen_linkEditParms', buildFile)

	// define the MVSExec command to link edit the program
	MVSExec linkedit = new MVSExec().file(buildFile).pgm(props.psbgen_linkEditor).parm(parameters)

	// add DD statements to the linkedit command
	String psbgen_loadPDS = props.getFileProperty('psbgen_loadPDS', buildFile)
	String psbgen_deployType = props.getFileProperty('psbgen_deployType', buildFile)
	if ( psbgen_deployType == null )
		psbgen_deployType = 'LOAD'
	linkedit.dd(new DDStatement().name("SYSLMOD").dsn("${psbgen_loadPDS}($member)").options('shr').output(true).deployType(psbgen_deployType))
	linkedit.dd(new DDStatement().name("SYSPRINT").options(props.psbgen_tempOptions))
	linkedit.dd(new DDStatement().name("SYSUT1").options(props.psbgen_tempOptions))

	// add a copy command to the linkedit command to append the SYSPRINT from the temporary dataset to the HFS log file
	linkedit.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))

	return linkedit
}


/*
 * createACBGenCommand - creates a MVSExec xommand to perform the ACBgen of DBD and PSB outputs
 */
def createACBgenCommand(String buildFile, String member, File logFile) {


	//         PROC SOUT='*',COMP=,RGN=4M,PLEX=IMSC,IMS=                    00010004
	//G        EXEC PGM=DFSRRC00,PARM='UPB,&COMP',REGION=&RGN               00020000
	//*                                                                     00030002
	//* PLEASE NOTE THAT DBDLIB AND PSBLIB ARE SHARED ACROSS THE PLEX,      00031002
	//* BUT THE ACBLIB IS NOT (BECAUSE IT IS VERSION-DEPENDENT)             00032002
	//*                                                                     00033002
	//SYSPRINT  DD SYSOUT=&SOUT                                             00040000
	//STEPLIB   DD DSN=IMSCFG.&IMS..SDFSRESL,DISP=SHR                       00050002
	//IMS       DD DSN=IMSCFG.&PLEX..PSBLIB,DISP=SHR                        00070001
	//          DD DSN=IMSCFG.&PLEX..DBDLIB,DISP=SHR                        00080001
	//IMSACB    DD DSN=IMSCFG.&IMS..ACBLIB,DISP=SHR                         00090005
	//SYSUT3    DD UNIT=SYSDA,SPACE=(80,(100,100))                          00100000
	//SYSUT4    DD UNIT=SYSDA,SPACE=(256,(100,100)),DCB=KEYLEN=8            00110000
	//COMPCTL   DD DISP=SHR,DSN=IMSCFG.&PLEX..PROCLIB(DFSACBCP)             00130001

	String parameters = props.getFileProperty('acbgen_pgmParms', buildFile)

	// define the MVSExec command for MFS Language Utility - Phase 2
	MVSExec acbgen = new MVSExec().file(buildFile).pgm(props.acbgen_pgm).parm(parameters)

	// add DD statements to the mfsPhase2 command
	// create a SYSLIB concatenation with optional MACLIB and MODGEN
	acbgen.dd(new DDStatement().name("IMS").dsn(props.acbgen_psbPDS).options("shr"))
	acbgen.dd(new DDStatement().dsn(props.acbgen_dbdPDS).options("shr"))

	//build instream dd to build PSB
	acbgen.dd(new DDStatement().name("SYSIN").instreamData("  BUILD PSB=($member)"))

	// adding Steplib
	acbgen.dd(new DDStatement().name("TASKLIB").dsn(props.SDFSRESL).options("shr"))

	// retrieve target pds and deploytype
	String acbgen_loadPDS = props.getFileProperty('acbgen_loadPDS', buildFile)
	String acbgen_deployType = props.getFileProperty('acbgen_deployType', buildFile)
	if ( acbgen_deployType == null )
		acbgen_deployType = 'LOAD'
	acbgen.dd(new DDStatement().name("IMSACB").dsn("${acbgen_loadPDS}").options('shr').output(true).deployType(acbgen_deployType))

	// addional allocations
	acbgen.dd(new DDStatement().name("SYSPRINT").options(props.psbgen_tempOptions))
	acbgen.dd(new DDStatement().name("SYSUT3").options(props.psbgen_tempOptions))
	acbgen.dd(new DDStatement().name("SYSUT4").options(props.psbgen_tempOptions))

	// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
	acbgen.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))

	return acbgen
}

def getRepositoryClient() {
	if (!repositoryClient && props."dbb.RepositoryClient.url")
		repositoryClient = new RepositoryClient().forceSSLTrusted(true)

	return repositoryClient
}



