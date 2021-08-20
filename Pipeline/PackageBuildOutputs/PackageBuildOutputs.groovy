import java.io.File
import java.io.UnsupportedEncodingException
import java.security.MessageDigest
import org.apache.http.entity.FileEntity
import com.ibm.dbb.build.*
import com.ibm.dbb.build.DBBConstants.CopyMode
import com.ibm.dbb.build.report.BuildReport
import com.ibm.dbb.build.report.records.DefaultRecordFactory
import groovy.transform.*

/************************************************************************************
 * This script creates a simplified package with the outputs generated from a DBB build 
 * Optionally, it publishes it to an Artifactory repository.
 *
 * usage: PackageBuildOutputs.groovy [options]
 *
 * -w,--workDir <dir>                    Absolute path to the DBB build
 *                                       output directory
 * Optional:
 * -t,--tarFileName <filename>           Name of the package tar file.
 *                                       (Optional)
 * -d,--deployTypes <deployTypes>        Comma-seperated list of deployTypes
 *                                       to filter on the scope of the tar
 *                                       file. (Optional)
 * -verb,--verbose                       Flag to provide more log output.
 *                                       (Optional)
 *                                       
 * Optional Artifactory Upload opts:
 *
 * -p,--publish                          Flag to indicate package upload to
 *                                       the provided Artifactory server.
 *                                       (Optional)
 * -prop,--propertyFile <propertyFile>   Absolute path of a property file
 *                                       containing application specific
 *                                       Artifactory details. (Optional)
 * -v,--versionName <versionName>        Name of the Artifactory version.
 *                                       (Optional)
 * -h,--help                             Prints this message
 *
 * Version 0 - 2019
 *  called PublishLoadModule.groovy and located in Build/PublishLoadModules
 *
 * Version 1 - 2021
 *  Re-Design to run as a post-build script and make publishing optional
 *
 ************************************************************************************/

// start create & publish package
@Field Properties props = parseInput(args)

// Map of last level dataset qualifier to DBB CopyToFS CopyMode.
// TODO: Customize to your needs. 
def copyModeMap = ["COPYBOOK": CopyMode.TEXT, "COPY": CopyMode.TEXT, "DBRM": CopyMode.BINARY, "LOAD": CopyMode.LOAD]


def startTime = new Date()
props.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** PackageBuildOutputs start at $props.startTime")
println("** Properties at startup:")
props.each{k,v->
	println "   $k -> $v"
}

// Enable file tagging
BuildProperties.setProperty("dbb.file.tagging", "true") // Enable dbb file tagging

// read build report data
println("** Read build report data from $props.workDir/BuildReport.json")
def jsonOutputFile = new File("${props.workDir}/BuildReport.json")

if(!jsonOutputFile.exists()){
	println("** Build report data at $props.workDir/BuildReport.json not found")
	System.exit(1)
}

def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))

// finds all the build outputs with a deployType
def executes= buildReport.getRecords().findAll{
	try {
		(it.getType()==DefaultRecordFactory.TYPE_EXECUTE || it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS) &&
				!it.getOutputs().isEmpty()
	} catch (Exception e){}
}

if (props.deployTypeFilter){
	println("** Filtering Output Records on following deployTypes: ${props.deployTypeFilter}")
	executes.each {
		// filtered executes
		def filteredOutputs =  it.getOutputs().findAll{ o ->
			o.deployType != null && (props.deployTypeFilter).split(',').contains(o.deployType)
		}
		// Manipulating the scope of build outputs
		it.getOutputs().clear()
		it.getOutputs().addAll(filteredOutputs)
	}
}
else {
	// Remove outputs without deployType + ZUNIT-TESTCASEs
	println("** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE ")
	executes.each {
		def unwantedOutputs =  it.getOutputs().findAll{ o ->
			o.deployType == null || o.deployType == 'ZUNIT-TESTCASE'
		}
		it.getOutputs().removeAll(unwantedOutputs)
	}
}

if (executes.size() == 0) { 
	println("*!* There are no outputs found in the build report.")
}
else {
	// Read buildInfo to obtain build information

	def buildInfo = buildReport.getRecords().findAll{
		try {
			it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT
		} catch (Exception e){}
	}

	def String tarFileLabel = buildInfo[0].label
	def String tarFileName = (props.tarFileName) ? props.tarFileName : "${buildInfo[0].label}.tar"
	def String buildGroup = buildInfo[0].group


	//Create a temporary directory on zFS to copy the load modules from data sets to
	def tempLoadDir = new File("$props.workDir/tempPackageDir")
	!tempLoadDir.exists() ?: tempLoadDir.deleteDir()
	tempLoadDir.mkdirs()

	//Iterate over executes and obtain map of <dataset,member>
	def loadDatasetToMembersMap = [:]
	def loadCount = 0
	executes.each { execute ->
		execute.outputs.each { output ->
			def (dataset, member) = output.dataset.split("\\(|\\)")
			if (loadDatasetToMembersMap[dataset] == null)
				loadDatasetToMembersMap[dataset] = []
			loadDatasetToMembersMap[dataset].add(member)
			loadCount++
		}
	}

	//For each load modules, use CopyToHFS with option 'CopyMode.LOAD' to maintain SSI
	println("** Copying BuildOutputs to temporary package dir.")

	CopyToHFS copy = new CopyToHFS()

	println "*** Number of build outputs to publish: $loadCount"
	loadDatasetToMembersMap.each { dataset, members ->
		members.each { member ->

			def fullyQualifiedDsn = "$dataset($member)"
			def filePath = "$tempLoadDir/$dataset"
			new File(filePath).mkdirs()
			def file = new File(filePath, member)

			// set copyMode based on last level qualifier
			currentCopyMode = copyModeMap[dataset.replaceAll(/.*\.([^.]*)/, "\$1")]
			copy.setCopyMode(currentCopyMode)
			copy.setDataset(dataset)

			println "     Copying $dataset($member) to $filePath with DBB Copymode $currentCopyMode"
			copy.dataset(dataset).member(member).file(file).execute()

			// Workaround DBB 1.1.1 toolkit
			if (currentCopyMode == CopyMode.BINARY || currentCopyMode == CopyMode.LOAD){
				StringBuffer stdout = new StringBuffer()
				StringBuffer stderr = new StringBuffer()

				Process process = "chtag -b $file".execute()
				process.waitForProcessOutput(stdout, stderr)
				if (stderr){
					println ("*! stderr : $stderr")
					println ("*! stdout : $stdout")
				}

			}
		}
	}

	def tarFile = new File("$props.workDir/${tarFileName}")

	println("** Creating tar file at $tarFile.")
	// Note: https://www.ibm.com/docs/en/zos/2.4.0?topic=scd-tar-manipulate-tar-archive-files-copy-back-up-file
	// To save all attributes to be restored on z/OS and non-z/OS systems : tar -UX
	def processCmd = [
		"sh",
		"-c",
		"tar cUXf $tarFile *"
	]

	def rc = runProcess(processCmd, tempLoadDir)
	assert rc == 0 : "Failed to package"

	//Package BuildReport.json to carry BuildInfo including deployTypes etc.
	println("** Adding BuildReport.json to $tarFile.")
	processCmd = [
		"sh",
		"-c",
		"tar rUXf $tarFile BuildReport.json"
	]

	rc = runProcess(processCmd, new File(props.workDir))
	assert rc == 0 : "Failed to append BuildReport.json"

	println ("** Package successfully created at $tarFile.")

	//Set up the artifactory information to publish the tar file
	if (props.publish && props.publish.toBoolean()){
		// Configuring ArtifactoryHelper parms
		def String remotePath = (props.versionName) ? (props.versionName + "/" + tarFileName) : (tarFileLabel + "/" + tarFileName)
		def url = new URI(props.get('artifactory.url') + "/" + props.get('artifactory.repo') + "/" + remotePath ).normalize().toString() // Normalized URL

		def apiKey = props.'artifactory.user'
		def user = props.'artifactory.user'
		def password = props.'artifactory.password'
		def repo = props.get('artifactory.repo') as String

		//Call the ArtifactoryHelpers to publish the tar file
		def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
		File artifactoryHelpersFile = new File("$scriptDir/ArtifactoryHelpers.groovy")
		Class artifactoryHelpersClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(artifactoryHelpersFile)
		GroovyObject artifactoryHelpers = (GroovyObject) artifactoryHelpersClass.newInstance()

		println ("** Uploading package to Artifactory $url.")
		artifactoryHelpers.upload(url, tarFile as String, user, password, props.verbose.toBoolean() )
	}

}

/**
 * parse data set name and member name
 * @param fullname e.g. BLD.LOAD(PGM1)
 * @return e.g. (BLD.LOAD, PGM1)
 */
def getDatasetName(String fullname){
	def ds,member;
	def elements =  fullname.split("[\\(\\)]");
	ds = elements[0];
	member = elements.size()>1? elements[1] : "";
	return [ds, member];
}

/**
 * run process
 */
def runProcess(ArrayList cmd, File dir){
	if (props.verbose && props.verbose.toBoolean()) println "executing $cmd: "
	StringBuffer response = new StringBuffer()
	StringBuffer error = new StringBuffer()

	// execute cmd
	def p = cmd.execute(null, dir)

	p.waitForProcessOutput(response, error)
	println(response.toString())

	def rc = p.exitValue();
	if(rc!=0){
		println("*! Error executing $cmd \n" + error.toString())
		//System.exit(1)
	}
	return rc
}

/**
 * read cliArgs
 */
def parseInput(String[] cliArgs){
	def cli = new CliBuilder(usage: "PackageBuildOutputs.groovy [options]")
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the DBB build output directory')
	cli.d(longOpt:'deployTypes', args:1, argName:'deployTypes','Comma-seperated list of deployTypes to filter on the scope of the tar file. (Optional)')
	cli.t(longOpt:'tarFileName', args:1, argName:'filename', 'Name of the package tar file. (Optional)')

	// Artifactory Options:
	cli.p(longOpt:'publish', 'Flag to indicate package upload to the provided Artifactory server. (Optional)')
	cli.v(longOpt:'versionName', args:1, argName:'versionName', 'Name of the Artifactory version. (Optional)')
	cli.prop(longOpt:'propertyFile', args:1, argName:'propertyFile', 'Absolute path of a property file containing application specific Artifactory details. (Optional)')
	cli.verb(longOpt:'verbose', 'Flag to provide more log output. (Optional)')

	cli.h(longOpt:'help', 'Prints this message')
	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
		cli.usage()
		System.exit(0)
	}

	def props = new Properties()

	// set command line arguments
	if (opts.w) props.workDir = opts.w
	if (opts.d) props.deployTypeFilter = opts.d
	if (opts.t) props.tarFileName = opts.t

	props.verbose = (opts.verb) ? 'true' : 'false'

	// Optional Artifactory to publish
	if (opts.v) props.versionName = opts.v
	props.publish = (opts.p) ? 'true' : 'false'

	if (opts.prop){
		def propertyFile = new File(opts.prop)
		if (propertyFile.exists()){
			propertyFile.withInputStream { props.load(it) }
		}
	}

	// validate required props
	try {
		assert props.workDir : "Missing property build work directory"
		if (props.publish && props.publish.toBoolean()){
			assert props.get("artifactory.url") : "Missing Artifactory URL"
			assert props.get("artifactory.repo") : "Missing Artifactory Repository"
			assert props.get("artifactory.user") : "Missing Artifactory Username"
			assert props.get("artifactory.password") : "Missing Artifactory Password"
		}

	} catch (AssertionError e) {
		cli.usage()
		throw e
	}
	return props
}