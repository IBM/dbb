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
buildUtils.assertBuildProperties(props.linkedit_requiredBuildProperties)

// sort the build list based on build file rank if provided
List<String> sortedList = buildUtils.sortBuildList(argMap.buildList, 'linkedit_fileBuildRank')

// iterate through build list
sortedList.each { buildFile ->
	println "*** Building file $buildFile"
	
	// copy build file to input data set
	buildUtils.copySourceFiles(buildFile, props.linkedit_srcPDS, null, null)
	
	// create mvs commands
	String rules = props.getFileProperty('linkedit_resolutionRules', buildFile)
	DependencyResolver dependencyResolver = buildUtils.createDependencyResolver(buildFile, rules)
	LogicalFile logicalFile = dependencyResolver.getLogicalFile()
	String member = CopyToPDS.createMemberName(buildFile)
	File logFile = new File("${props.buildOutDir}/${member}.log")
	MVSExec linkEdit = createLinkEditCommand(buildFile, logicalFile, member, logFile)
	
	// execute mvs commands in a mvs job
	MVSJob job = new MVSJob()
	job.start()
	
	rc = linkEdit.execute()
	maxRC = props.getFileProperty('linkedit_maxRC', buildFile).toInteger()
		
	if (rc > maxRC) {
		String errorMsg = "*! The link edit return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
		println(errorMsg)
		props.error = "true"
		buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
	}
	else {
		// only scan the load module if load module scanning turned on for file
		String scanLoadModule = props.getFileProperty('linkedit_scanLoadModule', buildFile)
		if (scanLoadModule && scanLoadModule.toBoolean() && getRepositoryClient())
			impactUtils.saveStaticLinkDependencies(buildFile, props.linkedit_loadPDS, logicalFile, repositoryClient)
	}
	
	job.stop()
}

// end script


//********************************************************************
//* Method definitions
//********************************************************************

/*
 * createLinkEditCommand - creates a MVSExec xommand for link editing the object module produced by link file
 */
def createLinkEditCommand(String buildFile, LogicalFile logicalFile, String member, File logFile) {
	String parms = props.getFileProperty('linkEdit_parms', buildFile)
	String linker = props.getFileProperty('linkedit_linkEditor', buildFile)
	
	// define the MVSExec command to link edit the program
	MVSExec linkedit = new MVSExec().file(buildFile).pgm(linker).parm(parms)
	
	// add DD statements to the linkedit command
	linkedit.dd(new DDStatement().name("SYSLIN").dsn("${props.linkedit_srcPDS}($member)").options("shr").report(true))
	linkedit.dd(new DDStatement().name("SYSLMOD").dsn("${props.linkedit_loadPDS}($member)").options('shr').output(true).deployType('LOAD'))
	linkedit.dd(new DDStatement().name("SYSPRINT").options(props.linkedit_tempOptions))
	linkedit.dd(new DDStatement().name("SYSUT1").options(props.linkedit_tempOptions))
	
	// add a syslib to the compile command with optional CICS concatenation
	linkedit.dd(new DDStatement().name("SYSLIB").dsn(props.linkedit_objPDS).options("shr"))
	linkedit.dd(new DDStatement().dsn(props.SCEELKED).options("shr"))
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




