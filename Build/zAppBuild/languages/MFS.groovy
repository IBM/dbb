@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*

// docs: https://www.ibm.com/support/knowledgecenter/en/SSEPH2_13.1.0/com.ibm.ims13.doc.sur/ims_mfslangbatchmd.htm

// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def buildUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BuildUtilities.groovy"))

@Field RepositoryClient repositoryClient

println("** Building files mapped to ${this.class.getName()}.groovy script")

// verify required build properties
buildUtils.assertBuildProperties(props.mfs_requiredBuildProperties)

// sort the build list based on build file rank if provided
List<String> sortedList = buildUtils.sortBuildList(argMap.buildList, 'mfs_fileBuildRank')

// iterate through build list
sortedList.each { buildFile ->
	println "*** Building file $buildFile"

	// copy build file to input data set
	buildUtils.copySourceFiles(buildFile, props.mfs_srcPDS, null, null)

	// create mvs commands
	String member = CopyToPDS.createMemberName(buildFile)
	File logFile = new File( props.userBuild ? "${props.buildOutDir}/${member}.log" : "${props.buildOutDir}/${member}.mfs.log")
	if (logFile.exists())
		logFile.delete()
	MVSExec phase1 = createPhase1Command(buildFile, member, logFile)
	MVSExec phase2 = createPhase2Command(buildFile, member, logFile)

	// execute mvs commands in a mvs job
	MVSJob job = new MVSJob()
	job.start()

	// preprocess mfs map
	int rc = phase1.execute()
	int maxRC = props.getFileProperty('mfs_phase1MaxRC', buildFile).toInteger()


	if (rc > maxRC) {
		String errorMsg = "*! The phase1 return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
		println(errorMsg)
		props.error = "true"
		buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
	}
	else {

		rc = phase2.execute()
		maxRC = props.getFileProperty('mfs_phase2MaxRC', buildFile).toInteger()

		if (rc > maxRC) {
			String errorMsg = "*! The phase 2 return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
			println(errorMsg)
			props.error = "true"
			buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
		}
	}

}

// end script


//********************************************************************
//* Method definitions
//********************************************************************


/*
 * createPhase1Command - creates a MVSExec command for preprocessing the MFS Map (buildFile)
 */
def createPhase1Command(String buildFile, String member, File logFile) {
	String parameters = props.getFileProperty('mfs_phase1Parms', buildFile)

	// define the MVSExec command to compile the mfs map
	MVSExec mfsPhase1 = new MVSExec().file(buildFile).pgm(props.mfs_phase1processor).parm(parameters)

	mfsPhase1.dd(new DDStatement().name("SYSIN").dsn("${props.mfs_srcPDS}($member)").options("shr").report(true))
	
	mfsPhase1.dd(new DDStatement().name("REFIN").dsn(props.REFERAL).options("shr"))
	mfsPhase1.dd(new DDStatement().name("REFOUT").dsn("&&TEMPPDS").options("${props.mfs_tempOptions} dir(5) lrecl(80) recfm(f,b)"))
	
	mfsPhase1.dd(new DDStatement().name("REFRD").dsn(props.REFERAL).options("shr"))
	mfsPhase1.dd(new DDStatement().name("SYSPRINT").options(props.mfs_tempOptions))
	mfsPhase1.dd(new DDStatement().name("SEQBLKS").dsn("&&SEQBLK").options(props.mfs_tempOptions).pass(true))
	mfsPhase1.dd(new DDStatement().name("SYSLIB").dsn(props.SDFSMAC).options("shr"))
	mfsPhase1.dd(new DDStatement().name("TASKLIB").dsn(props.SDFSRESL).options("shr"))

	mfsPhase1.dd(new DDStatement().name("SYSTEXT").dsn("&&TXTPASS").options(props.mfs_tempOptions))
	
	mfsPhase1.dd(new DDStatement().name("SYSUT3").options(props.mfs_tempOptions))
	mfsPhase1.dd(new DDStatement().name("SYSUT4").options(props.mfs_tempOptions))
	
	mfsPhase1.dd(new DDStatement().name("UTPRINT").options(props.mfs_tempOptions))

	// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
	mfsPhase1.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(props.logEncoding))

	return mfsPhase1
}


/*
 * createPhase2Command - creates a MVSExec xommand for running phase 2 of the mfs object module produced by phase 1
 */
def createPhase2Command(String buildFile, String member, File logFile) {
	
	String parameters = props.getFileProperty('mfs_phase2Parms', buildFile)

	// define the MVSExec command for MFS Language Utility - Phase 2
	MVSExec mfsPhase2 = new MVSExec().file(buildFile).pgm(props.mfs_phase2processor).parm(parameters)

	// add DD statements to the mfsPhase2 command
	mfsPhase2.dd(new DDStatement().name("UTPRINT").options(props.mfs_tempOptions))
	
	String mfs_deployType = props.getFileProperty('mfs_deployType', buildFile)
	if ( mfs_deployType == null )
		mfs_deployType = 'LOAD'
	
	mfsPhase2.dd(new DDStatement().name("FORMAT").dsn(props.mfs_tformatPDS).options("shr").output(true).deployType(mfs_deployType))
	mfsPhase2.dd(new DDStatement().name("TASKLIB").dsn(props.SDFSRESL).options("shr"))

	// add a copy command to the compile command to copy the SYSPRINT from the temporary dataset to an HFS log file
	mfsPhase2.copy(new CopyToHFS().ddName("UTPRINT").file(logFile).hfsEncoding(props.logEncoding).append(true))
}

def getRepositoryClient() {
	if (!repositoryClient && props."dbb.RepositoryClient.url")
		repositoryClient = new RepositoryClient().forceSSLTrusted(true)

	return repositoryClient
}



