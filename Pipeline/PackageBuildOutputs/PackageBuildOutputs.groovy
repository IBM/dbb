@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import java.io.File
import com.ibm.dbb.build.CopyToHFS
import com.ibm.dbb.build.BuildProperties
import com.ibm.dbb.build.DBBConstants
import com.ibm.dbb.build.DBBConstants.CopyMode
import com.ibm.dbb.build.report.BuildReport
import com.ibm.dbb.build.report.records.*
import groovy.transform.*
import groovy.cli.commons.*
import java.nio.file.*
import static java.nio.file.StandardCopyOption.*

/************************************************************************************
 * This script creates a simplified package with the outputs generated from a DBB build
 * Optionally, it publishes it to an Artifact repository.
 *
 * usage: see help command or README
 *
 * Version 0 - 2019
 *  called PublishLoadModule.groovy and located in Build/PublishLoadModules
 *
 * Version 1 - 2021
 *  Re-Design to run as a post-build script and make publishing optional
 *
 * Version 2 - 2022-02
 *  - Externalize the Map of LLQ to CopyMode
 *  - Add capablity to add additional files from build workspace
 *  - Verbose logging will print tar contents
 *
 * Version 3 - 2022-08
 *  - Ability to pass multiple build reports to build a cumulative package
 *  - Add an optional option to add the deploy type extension to the member
 *
 * Version 4 - 2022-12
 *  - Generalized the options to publish package to artifact repository
 *  - Script is supporting both JFrog Artifactory and SonarType Nexus
 *  - Added additional CLI option to pass artifact repository parameters
 *
 * Version 5 - 2023-04
 *  - Enhanced the multiple build reports feature to allow replacing the same artifact
 *  if memberName and deployType are the same. The key in the HashMap is changed to
 *  a DeployableArtifact object. 

 ************************************************************************************/

class DeployableArtifact {
	private final String file;
	private final String deployType;
	
	DeployableArtifact(String file, String deployType) {
		this.file = file;
		this.deployType = deployType;
	}

	@Override
	public int hashCode() {
		String concatenation = file + "." + deployType;
		return concatenation.hashCode();
	}

	public boolean equals(DeployableArtifact other) {
		return other.file.equals(file) & other.deployType.equals(deployType);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DeployableArtifact) {
			return equals((DeployableArtifact) other)
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return file + "." + deployType;
	}
}

// start create & publish package
@Field Properties props = null
props = parseInput(args)

def startTime = new Date()
props.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** PackageBuildOutputs start at $props.startTime")
println("** Properties at startup:")
props.sort().each{k,v->
	if ( k == "artifactRepository.password" )
		println "   $k -> xxxxxx "
	else
		println "   $k -> $v"
}

// Enable file tagging
BuildProperties.setProperty("dbb.file.tagging", "true") // Enable dbb file tagging

// Map of last level dataset qualifier to DBB CopyToFS CopyMode.
def copyModeMap = evaluate(props.copyModeMap)

// Hashmap of BuildOutput to Record
Map<DeployableArtifact, Map> buildOutputsMap = new HashMap<DeployableArtifact, Map>()

// Field to store default tarFileLabel (buildInfo.label) when cli argument tarFileName is not passed.
def String tarFileLabel = "Default"

// iterate over all build reports to obtain build output
props.buildReportOrder.each{ buildReportFile ->
	println("** Read build report data from ${buildReportFile}.")
	def jsonOutputFile = new File(buildReportFile)

	if(!jsonOutputFile.exists()){
		println("*! Error: Build report data at $buildReportFile not found.")
		System.exit(1)
	}

	def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))

	// Read buildInfo to obtain build information
	def buildInfo = buildReport.getRecords().findAll{
		try {
			it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT
		} catch (Exception e){}
	}
	if (buildInfo.size() != 0) {
		tarFileLabel = buildInfo[0].label
	}

	// finds all the build outputs with a deployType
	def buildRecords = buildReport.getRecords().findAll{
		try {
			(it.getType()==DefaultRecordFactory.TYPE_EXECUTE || it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS) &&
					!it.getOutputs().isEmpty()
		} catch (Exception e){}
	}

	if (props.deployTypeFilter){
		println("** Filtering Output Records on following deployTypes: ${props.deployTypeFilter}...")
		buildRecords.each {
			// filtered executes
			def filteredOutputs =  it.getOutputs().findAll{ o ->
				o.deployType != null && (props.deployTypeFilter).split(',').contains(o.deployType)
			}
			// Manipulating the scope of build outputs
			it.getOutputs().clear()
			it.getOutputs().addAll(filteredOutputs)
		}
	} else {
		// Remove outputs without deployType + ZUNIT-TESTCASEs
		println("** Removing Output Records without deployType or with deployType=ZUNIT-TESTCASE...")
		buildRecords.each {
			def unwantedOutputs =  it.getOutputs().findAll{ o ->
				o.deployType == null || o.deployType == 'ZUNIT-TESTCASE'
			}
			it.getOutputs().removeAll(unwantedOutputs)
		}
	}

	def count = 0

	// adding files and executes with outputs to Hashmap to remove redundant data
	buildRecords.each{ buildRecord ->
		if (buildRecord.getOutputs().size() != 0) {
			buildRecord.getOutputs().each{ output ->
				count++
				def (dataset, member) = getDatasetName(output.dataset)
				buildOutputsMap.put(new DeployableArtifact(member, output.deployType), [dataset, buildRecord])
			}
		}
	}

	if ( count == 0 ) {
		println("** No items to package in ${buildReportFile}.")
	} else {
		println("** Files detected in ${buildReportFile}.")
		buildRecords.each { it.getOutputs().each { println("   ${it.dataset}, ${it.deployType}")}}
	}
}

if (buildOutputsMap.size() == 0) {
	println("*! There are no build outputs found in all provided build reports.")
	System.exit(2)
} else {
	def String tarFileName = (props.tarFileName) ? props.tarFileName  : "${tarFileLabel}.tar"

	//Create a temporary directory on zFS to copy the load modules from data sets to
	def tempLoadDir = new File("$props.workDir/tempPackageDir")
	!tempLoadDir.exists() ?: tempLoadDir.deleteDir()
	tempLoadDir.mkdirs()

	println( "*** Number of build outputs to package: ${buildOutputsMap.size()}")

	println("** Copying build outputs to temporary package directory....")

	buildOutputsMap.each { deployableArtifact, info ->
		String dataset = info[0]
		Record record = info[1]
		
		def filePath = "$tempLoadDir/$dataset"
		new File(filePath).mkdirs()

		// define file name in USS
		// default : member
		def fileName = deployableArtifact.file

		// add deployType to file name
		if (props.addExtension && props.addExtension.toBoolean()) {
			fileName = fileName + '.' + deployableArtifact.deployType
		}
		def file = new File(filePath, fileName)

		// set copyMode based on last level qualifier
		currentCopyMode = copyModeMap[dataset.replaceAll(/.*\.([^.]*)/, "\$1")]
		if (currentCopyMode != null) {
			// Copy outputs to HFS
			CopyToHFS copy = new CopyToHFS()
			copy.setCopyMode(DBBConstants.CopyMode.valueOf(currentCopyMode))
			copy.setDataset(dataset)

			println "     Copying $dataset(${deployableArtifact.file}) to $filePath/$fileName with DBB Copymode $currentCopyMode"
			copy.dataset(dataset).member(deployableArtifact.file).file(file).execute()

			// Tagging binary files
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
		} else {
			println "!    Copying $dataset(${deployableArtifact.file}) could not be copied due to missing mapping."
		}

	}

	// log buildReportOrder file and add build reports to tar file
	File buildReportOrder = new File("$tempLoadDir/buildReportOrder.txt")
	buildReportOrder.write('')
	String logEncoding = 'UTF-8'
	String buildReportFileName
	int counter = 0

	buildReportOrder.withWriter(logEncoding) { writer ->
		props.buildReportOrder.each{ buildReportFile ->
			counter++
			
			Path buildReportFilePath = Paths.get(buildReportFile)
			Path copiedBuildReportFilePath = Paths.get(tempLoadDir.getPath() + "/" + buildReportFilePath.getFileName().toString())
			
			// prefixing the buildreport with sequence number when having multiple
			if (props.buildReportOrder.size() > 1)
				copiedBuildReportFilePath = Paths.get(tempLoadDir.getPath() + "/" + "$counter".padLeft(3, "0") + "_" + buildReportFilePath.getFileName().toString())

			Files.copy(buildReportFilePath, copiedBuildReportFilePath, COPY_ATTRIBUTES)
			writer.write("${copiedBuildReportFilePath.toString()}\n")
		}
	}

	Path packagingPropertiesFilePath = Paths.get(props.packagingPropertiesFile)
	Path copiedPackagingPropertiesFilePath = Paths.get(tempLoadDir.getPath() + "/" + packagingPropertiesFilePath.getFileName().toString())
	Files.copy(packagingPropertiesFilePath, copiedPackagingPropertiesFilePath, COPY_ATTRIBUTES)

	def tarFile = new File("$props.workDir/${tarFileName}")

	println("** Creating tar file at ${tarFile}...")
	// Note: https://www.ibm.com/docs/en/zos/2.4.0?topic=scd-tar-manipulate-tar-archive-files-copy-back-up-file
	// To save all attributes to be restored on z/OS and non-z/OS systems : tar -UX
	def processCmd = [
		"sh",
		"-c",
		"tar cUXf $tarFile *"
	]

	def rc = runProcess(processCmd, tempLoadDir)
	assert rc == 0 : "Failed to package"

	//Package additional outputs to tar file.
	if (props.includeLogs) (props.includeLogs).split(",").each { logPattern ->
		println("** Adding $logPattern to ${tarFile}...")
		processCmd = [
			"sh",
			"-c",
			"tar rUXf $tarFile $logPattern"
		]

		rc = runProcess(processCmd, new File(props.workDir))
		assert rc == 0 : "Failed to append $logPattern."
	}

	println ("** Package successfully created at ${tarFile}.")

	if(props.verbose && props.verbose.toBoolean()) {
		println ("**   List package contents.")

		processCmd = [
			"sh",
			"-c",
			"tar tvf $tarFile"
		]

		rc = runProcess(processCmd, new File(props.workDir))
		assert rc == 0 : "Failed to list contents of tarfile $tarFile."

	}

	//Set up the artifact repository information to publish the tar file
	if (props.publish && props.publish.toBoolean()){
		// Configuring artifact repositoryHelper parms
		def String remotePath = (props.versionName) ? (props.versionName + "/" + tarFileName) : (tarFileLabel + "/" + tarFileName)
		def url = new URI(props.get('artifactRepository.url') + "/" + props.get('artifactRepository.repo') + "/" + props.'artifactRepository.directory' + "/" + remotePath ).normalize().toString() // Normalized URL

		def apiKey = props.'artifactRepository.user'
		def user = props.'artifactRepository.user'
		def password = props.'artifactRepository.password'
		def repo = props.get('artifactRepository.repo') as String

		//Call the artifactRepositoryHelpers to publish the tar file
		def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
		File artifactRepoHelpersFile = new File("$scriptDir/ArtifactRepositoryHelpers.groovy")
		Class artifactRepositoryHelpersClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(artifactRepoHelpersFile)
		GroovyObject artifactRepositoryHelpers = (GroovyObject) artifactRepositoryHelpersClass.newInstance()

		println ("** Uploading package to Artifact Repository $url...")
		artifactRepositoryHelpers.upload(url, tarFile as String, user, password, props.verbose.toBoolean() )
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
	if (props.verbose && props.verbose.toBoolean()) println " Executing $cmd: "
	StringBuffer response = new StringBuffer()
	StringBuffer error = new StringBuffer()

	// execute cmd
	def p = cmd.execute(null, dir)

	p.waitForProcessOutput(response, error)
	if(response) println(response.toString())

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
	// required packaging options
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the DBB build output directory')
	cli.properties(longOpt:'packagingPropertiesFile', args:1, argName:'packagingPropertiesFile', 'Path of a property file containing application specific packaging details.')

	// optional packaging options
	cli.d(longOpt:'deployTypes', args:1, argName:'deployTypes','Comma-seperated list of deployTypes to filter on the scope of the tar file. (Optional)')
	cli.t(longOpt:'tarFileName', args:1, argName:'filename', 'Name of the package tar file. (Optional unless using --buildReportOrder or --buildReportOrderFile)')
	cli.il(longOpt:'includeLogs', args:1, argName:'includeLogs', 'Comma-separated list of files/patterns from the USS build workspace. (Optional)')
	cli.ae(longOpt:'addExtension', 'Flag to add the deploy type extension to the member in the package tar file. (Optional)')

	// Artifact repository options ::
	cli.p(longOpt:'publish', 'Flag to indicate package upload to the provided Artifact Repository server. (Optional)')
	cli.v(longOpt:'versionName', args:1, argName:'versionName', 'Name of the version/package on the Artifact repository server. (Optional)')
	
	// Artifact repository info
	cli.au(longOpt:'artifactRepositoryUrl', args:1, argName:'url', 'URL to the Artifact repository server. (Optional)')
	cli.ar(longOpt:'artifactRepositoryName', args:1, argName:'repoName', 'Artifact repository name to store the build. (Optional)')
	cli.ad(longOpt:'artifactRepositoryDirectory', args:1, argName:'repoDirectory', 'Directory path in the repository to store the build . (Optional)')
	cli.aU(longOpt:'artifactRepositoryUser', args:1, argName:'user', 'User to connect to the Artifact repository server. (Optional)')
	cli.aP(longOpt:'artifactRepositoryPassword', args:1, argName:'password', 'Password to connect to the Artifact repository server. (Optional)')
	cli.aprop(longOpt:'artifactRepositoryPropertyFile', args:1, argName:'propertyFile', 'Path of a property file containing application specific artifact repository details. (Optional) ** (Deprecated)')
	
	// old prop option (deprecated)
	cli.artifactory(longOpt:'artifact repositoryPropertiesFile', args:1, argName:'Artifactory repositoryPropertiesFile', 'Path of a property file containing application specific Artifactory repository details. (Optional) ** (Deprecated)')
	cli.prop(longOpt:'propertyFile', args:1, argName:'propertyFile', 'Path of a property file containing application specific artifact repository details. (Optional) ** (Deprecated)')

	cli.verb(longOpt:'verbose', 'Flag to provide more log output. (Optional)')

	// multiple build reports
	cli.boFile(longOpt:'buildReportOrderFile', args:1, argName:'buildReportOrderFile', 'A file that lists build reports in order of processing')
	cli.bO(longOpt:'buildReportOrder', args:1, argName:'buildReportOrder', 'List of build reports in order of processing ')


	cli.h(longOpt:'help', 'Prints this message')
	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
		cli.usage()
		System.exit(2)
	}

	def props = new Properties()

	// read properties file
	if (opts.properties){
		def propertiesFile = new File(opts.properties)
		if (propertiesFile.exists()){
			props.packagingPropertiesFile = opts.properties
			propertiesFile.withInputStream { props.load(it) }
		}
	} else { // read default sample properties file shipped with the script
		def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
		def defaultPackagePropFile = new File("$scriptDir/packageBuildOutputs.properties")
		if (defaultPackagePropFile.exists()){
			props.packagingPropertiesFile = "$scriptDir/packageBuildOutputs.properties"
			defaultPackagePropFile.withInputStream { props.load(it) }
		}
	}

	// set command line arguments
	if (opts.w) props.workDir = opts.w
	if (opts.d) props.deployTypeFilter = opts.d
	if (opts.t) props.tarFileName = opts.t
	if (opts.il) props.includeLogs = opts.il
	props.addExtension = (opts.ae) ? 'true' : 'false'

	props.verbose = (opts.verb) ? 'true' : 'false'

	// Optional Artifact repository info to deploy package
	
	if (opts.v) props.versionName = opts.v
	props.publish = (opts.p) ? 'true' : 'false'
	
	// read of artifact repository file
	if (opts.aprop){
		def propertyFile = new File(opts.aprop)
		if (propertyFile.exists()){
			propertyFile.withInputStream { props.load(it) }
		}
	}
	
	// read the artifact repo cli options, which take precedence over
	// the properties file

	if (opts.aU) props.'artifactRepository.user' = opts.aU
	if (opts.aP) props.'artifactRepository.password' = opts.aP
	if (opts.au) props.'artifactRepository.url' = opts.au
	if (opts.ar) props.'artifactRepository.repo' = opts.ar
	if (opts.ad) props.'artifactRepository.directory' = opts.ad
	
	
	
	//  ** Deprecated ** Optional artifact repository properties
	// properties require to follow new naming conventions artifactRepository.*
	if (opts.artifactory){
		def propertyFile = new File(opts.artifactory)
		if (propertyFile.exists()){
			propertyFile.withInputStream { props.load(it) }
		}
	}

	// ** Deprecated ** Read of artifact repository properties
	if (opts.prop){
		def propertyFile = new File(opts.prop)
		if (propertyFile.exists()){
			propertyFile.withInputStream { props.load(it) }
		}
	}

	//add any build reports from the file first, then add any from a CLI after.
	//if no file or CLI, go to default build report
	def buildReports = []
	if (opts.boFile) {
		new File (opts.boFile).eachLine { line ->
			buildReports.add(line)
		}

		if(opts.t == false) {
			println("*! Error: tarFilename is only optional when no build report order is specified")
			System.exit(3)
		}

	}
	if (opts.bO) {
		opts.bO.split(',').each{
			buildReports.add(it)
		}
		if(opts.t == false) {
			println("*! Error: tarFilename is only optional when no build report order is specified")
			System.exit(3)
		}
	} else if (buildReports.isEmpty()){
		buildReports = [props.workDir + "/BuildReport.json"]
	}
	props.buildReportOrder = buildReports



	// validate required props
	try {
		assert props.workDir : "Missing property build work directory"
		assert props.copyModeMap : "Missing property package.copyModeMap"
		
		if (props.publish && props.publish.toBoolean()){
			assert props.get("artifactRepository.url") : "Missing artifact repository URL"
			assert props.get("artifactRepository.repo") : "Missing artifact repository name"
			assert props.get("artifactRepository.user") : "Missing artifact repository Username"
			assert props.get("artifactRepository.password") : "Missing artifact repository Password"
		}

	} catch (AssertionError e) {
		cli.usage()
		throw e
	}
	return props
}

/*
 * relativizePath - converts an absolute path to a relative path from the workspace directory
 */
def relativizePath(String path) {
	if (!path.startsWith('/'))
		return path
	String relPath = new File(props.workDir).toURI().relativize(new File(path.trim()).toURI()).getPath()
	// Directories have '/' added to the end.  Lets remove it.
	if (relPath.endsWith('/'))
		relPath = relPath.take(relPath.length()-1)
	return relPath
}