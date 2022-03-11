import com.ibm.dbb.build.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.time.*
import groovy.cli.commons.*
import com.ibm.dbb.build.VersionInfo
import groovy.xml.MarkupBuilder
/**
 * This script creates a version in UrbanCode Deploy based on the build result.
 *
 * usage: dbb-ucd-packaging.groovy [options]
 *
 * options:
 *  -b,--buztool <file>           Absolute path to UrbanCode Deploy buztool.sh script
 *  -w,--workDir <dir>            Absolute path to the DBB build output directory
 *  -c,--component <name>         Name of the UCD component to create version in
 *  -v,--versionName <name>       Name of the UCD component version name (Optional)
 *  -h,--help                     Prints this message
 *  -ar,--artifactRepository      Absolute path to Artifact Respository Server connection file (Optional)
 *  -prop,--propertyFile          Absolute path to property file (Optional). From UCD v7.1.x and greater it replace the -ar option.
 *  -p,--preview                  Preview, not executing buztool.sh
 *  -pURL,--pipelineURL			  URL to the pipeline build result (Optional)
 *  -g,--gitBranch				  Name of the git branch (Optional)
 *  -rpFile,--repositoryInfoPropertiesFile  Absolute path to property file containing URL prefixes to git provider (Optional).
 *
 * notes:
 *   This script uses ship list specification and buztool parameters which are
 *   introduced since UCD v7.0.4.
 *
 *   When used with an earlier version of UCD, please
 *   modify the script to remove the code that creates the top level property and
 *   the code that uses the -o buztool parameter.
 *
 * Version 0 - 2019
 *  originally called deploy.groovy
 *
 * Version 1 - 2020-01
 *  New option to support new UCD + Artifactory Support
 *
 * Version 2 - 2020-06
 *  Added option to define UCD component version name (optional)
 *  Option -ar now optional; renamed to --artifactRepository, 
 *   now supporting both external artifact repository (Artifactory/Nexus) + UCD Codestation
 *  Added preview option to skip buztool.sh execution
 *
 * Version 3 - 2020-08
 *  Fix ucd component version property for buildResultUrl
 *  Added support for build outputs declared in a CopyToPDS Build Record (JCL, REXX, Shared copybooks, etc.)
 *    Keep backward compatibility with older toolkits
 *
 * Version 4 - 2020-11
 *  Take into account new properties for Artifact Respository Server connection.
 *
 * Version 5 - 2021-04
 *  Take into account https://github.com/IBM/dbb/issues/76.
 *  
 * Version 6 - 2021-06
 *  Take into account  https://github.com/IBM/dbb/issues/78
 */

// start create version
def properties = parseInput(args)
def startTime = new Date()
properties.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** Create version start at $properties.startTime")
println("** Properties at startup:")
properties.each{k,v->
	println "   $k -> $v"
}

// read build report data
println("** Read build report data from $properties.workDir/BuildReport.json")
def jsonOutputFile = new File("${properties.workDir}/BuildReport.json")

if(!jsonOutputFile.exists()){
	println("** Build report data at $properties.workDir/BuildReport.json not found")
	System.exit(1)
}

def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))

// parse build report to find the build result meta info
def buildResult = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT}[0];

// parse build report to find the build outputs to be deployed.
println("** Find deployable outputs in the build report ")

// the following example finds all the build outputs with and without deployType
//def executes= buildReport.getRecords().findAll{
//	it.getType()==DefaultRecordFactory.TYPE_EXECUTE
//}

// Print warning, that extraction of COPY_TO_PDS records are not supported with older versions of the dbb toolkit.
dbbVersion = new VersionInfo().getVersion()
println "   * Buildrecord type TYPE_COPY_TO_PDS is supported with DBB toolkit 1.0.8 and higher. Extracting build records for TYPE_COPY_TO_PDS might not be available and skipped. Identified DBB Toolkit version $dbbVersion."

// finds all the build outputs with a deployType
def executes= buildReport.getRecords().findAll{
	try {
		(it.getType()==DefaultRecordFactory.TYPE_EXECUTE || it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS) &&
				!it.getOutputs().isEmpty()
	} catch (Exception e){}
}

// Remove excluded outputs
executes.each {
	def unwantedOutputs =  it.getOutputs().findAll{ o ->
		o.deployType == null || o.deployType == 'ZUNIT-TESTCASE'
	}
	it.getOutputs().removeAll(unwantedOutputs)
}

def count = 0
executes.each { count += it.getOutputs().size() }

if ( count == 0 ) {
	println("** No items to deploy. Skipping ship list generation.")
	System.exit(0)
}

executes.each { it.getOutputs().each { println("   ${it.dataset}, ${it.deployType}")}}

// get DBB.BuildResultProperties records stored as generic DBB Record, see https://github.com/IBM/dbb-zappbuild/pull/95
def buildResultRecord = buildReport.getRecords().find{
	try {
		it.getType()==DefaultRecordFactory.TYPE_PROPERTIES && it.getId()=="DBB.BuildResultProperties"
	} catch (Exception e){}
}

def buildResultProperties = null

if(buildResultRecord!=null){
	buildResultProperties = buildResultRecord.getProperties()
}

// generate ship list file. specification of UCD ship list can be found at
// https://www.ibm.com/support/knowledgecenter/SS4GSP_6.2.7/com.ibm.udeploy.doc/topics/zos_shiplistfiles.html
println("** Generate UCD ship list file")
def writer = new StringWriter()
writer.write("<?xml version=\"1.0\" encoding=\"CP037\"?>\n");
def xml = new MarkupBuilder(writer)
xml.manifest(type:"MANIFEST_SHIPLIST"){
	//top level property will be added as version properties
	//requires UCD v6.2.6 and above
	// Url to DBB Build result
	property(name : "dbb-buildResultUrl", value : buildResult.getUrl())
	// Url to CI pipeline
	if (properties.pipelineURL) property(name : "ci-pipelineUrl", value : properties.pipelineURL )
	// Git branch
	if (properties.gitBranch) property(name : "ci-gitBranch", value : properties.gitBranch )
	// Populate build result properties
	if (buildResultProperties != null) buildResultProperties.each{
		property(name:it.key, value:it.value)
	}
	//iterate through the outputs and add container and resource elements
	executes.each{ execute ->
		execute.getOutputs().each{ output ->
			def (ds,member) = getDatasetName(output.dataset)
			container(name:ds, type:"PDS"){
				resource(name:member, type:"PDSMember", deployType:output.deployType){
					// add any custom properties needed
					property(name:"buildcommand", value:execute.getCommand())

					// Only TYPE_EXECUTE Records carry options
					if (execute.getType()==DefaultRecordFactory.TYPE_EXECUTE) property(name:"buildoptions", value:execute.getOptions())
					// Sample to add additional properties. Here: adding db2 properties for a DBRM
					//   which where added to the build report through a basic PropertiesRecord.
					if (output.deployType.equals("DBRM")){
						propertyRecord = buildReport.getRecords().findAll{
							it.getType()==DefaultRecordFactory.TYPE_PROPERTIES && it.getProperty("file")==execute.getFile()
						}
						propertyRecord.each { propertyRec ->
							// Iterate Properties
							(propertyRec.getProperties()).each {
								property(name:"$it.key", value:it.value)

							}
						}
					}
					def githash = "" // set empty
					if (buildResultProperties != null){
						// get git references from build properties
						def gitproperty = buildResultProperties.find{
							it.key.contains(":githash:") && execute.getFile().contains(it.key.substring(9))
						}
						if (gitproperty != null ) {
							githash = gitproperty.getValue()
							// set properties in shiplist
							property(name:"githash", value:githash)
							if(properties.git_commitURL_prefix) property(name:"git-link-to-commit", value:"${properties.git_commitURL_prefix}/${githash}")
						}
					}
					// add source information in the input column
					inputUrl = (buildResultProperties != null && properties.git_treeURL_prefix && githash!="") ? "${properties.git_treeURL_prefix}/${githash}/"+ execute.getFile() : ""
					inputs(url : "${inputUrl}"){
						input(name : execute.getFile(), compileType : "Main", url : inputUrl)
						// dependencies
						def dependencySets = buildReport.getRecords().findAll{
							it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET && it.getFile()==execute.getFile()
						};
						Set<String> dependencyCache = new HashSet<String>()
						dependencySets.unique().each{
							it.getAllDependencies().each{
								if (it.isResolved() && !dependencyCache.contains(it.getLname()) && it.getFile()!=execute.getFile()){
									def displayName = it.getFile() ? it.getFile() : it.getLname()
									def dependencyUrl =""
									if (it.getFile() && (it.getCategory()=="COPY"||it.getCategory()=="SQL INCLUDE")) dependencyUrl = (buildResultProperties != null && properties.git_treeURL_prefix && githash!="") ? "${properties.git_treeURL_prefix}/${githash}/"+ it.getFile() : ""
									input(name : displayName , compileType : it.getCategory(), url : dependencyUrl)
									dependencyCache.add(it.getLname())
								}
							}
						}
					}
				}
			}
		}
	}
}
println("** Write ship list file to  $properties.workDir/shiplist.xml")
def shiplistFile = new File("${properties.workDir}/shiplist.xml")
shiplistFile.text = writer

// assemble and run UCD buztool command to create a version. An example of the command is like below
// /opt/ibm-ucd/agent/bin/buztool.sh createzosversion -c MYCOMP -s /var/dbb/workDir/shiplist.xml
// command parameters can be found at
// https://www.ibm.com/support/knowledgecenter/SS4GSP_6.2.7/com.ibm.udeploy.doc/topics/zos_runtools_uss.html

def cmd = [
	properties.buztoolPath,
	"createzosversion",
	"-c",
	properties.component,
	"-s",
	"$properties.workDir/shiplist.xml",
	//requires UCD v6.2.6 and above
	"-o",
	"${properties.workDir}/buztool.output"
]
// set artifactRepository option if specified
if (properties.artifactRepositorySettings) {
	cmd << "-ar"
	cmd << properties.artifactRepositorySettings
}

// set propertyFile option if specified
if (properties.propertyFileSettings) {
	cmd << "-prop"
	cmd << properties.propertyFileSettings
}

//set component version name if specified
if(properties.versionName){
	cmd << "-v"
	cmd <<  properties.versionName
}

def cmdStr = "";
cmd.each{ cmdStr = cmdStr + it + " "}
println("** Following UCD buztool cmd will be invoked")
println cmdStr

// execute command, if no preview is set
if (!properties.preview.toBoolean()){


	println("** Create version by running UCD buztool")

	StringBuffer response = new StringBuffer()
	StringBuffer error = new StringBuffer()

	def p = cmd.execute()
	p.waitForProcessOutput(response, error)
	println(response.toString())

	def rc = p.exitValue();
	if(rc==0){
		println("** buztool output properties")
		def outputProp = new Properties()
		new File("${properties.workDir}/buztool.output").withInputStream { outputProp.load(it) }
		outputProp.each{k,v->
			println "   $k -> $v"
		}
	}else{
		println("*! Error executing buztool\n" +error.toString())
		System.exit(rc)
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

def parseInput(String[] cliArgs){
	def cli = new CliBuilder(usage: "deploy.groovy [options]")
	cli.b(longOpt:'buztool', args:1, argName:'file', 'Absolute path to UrbanCode Deploy buztool.sh script')
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the DBB build output directory')
	cli.c(longOpt:'component', args:1, argName:'name', 'Name of the UCD component to create version in')
	cli.ar(longOpt:'artifactRepository', args:1, argName:'artifactRepositorySettings', 'Absolute path to Artifactory Server connection file')
	cli.prop(longOpt:'propertyFile', args:1, argName:'propertyFileSettings', 'Absolute path to property file (Optional). From UCD v7.1.x and greater it replace the -ar option')
	cli.v(longOpt:'versionName', args:1, argName:'versionName', 'Name of the UCD component version')
	cli.p(longOpt:'preview', 'Preview mode - generate shiplist, but do not run buztool.sh')
	cli.pURL(longOpt:'pipelineURL', args:1,'URL to the pipeline build result (Optional)')
	cli.g(longOpt:'gitBranch', args:1,'Name of the git branch (Optional)')
	cli.rpFile(longOpt:'repositoryInfoPropertiesFile', args:1,'Absolute path to property file containing URL prefixes to git provider (Optional)')

	cli.h(longOpt:'help', 'Prints this message')
	def opts = cli.parse(cliArgs)
	if (opts.h) { // if help option used, print usage and exit
		cli.usage()
		System.exit(0)
	}

	def properties = new Properties()

	// load workDir from ./build.properties if it exists
	def buildProperties = new Properties()
	def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
	def buildPropFile = new File("$scriptDir/build.properties")
	if (buildPropFile.exists()){
		buildPropFile.withInputStream { buildProperties.load(it) }
		if (buildProperties.workDir != null)
			properties.workDir = buildProperties.workDir
	}

	// load properties from repositoryInfoPropertiesFile
	if (opts.rpFile){
		def repositoryPropFile = new File("$opts.rpFile")
		if (repositoryPropFile.exists()){
			repositoryPropFile.withInputStream {  properties.load(it) }
		}
	}

	// set command line arguments
	if (opts.w) properties.workDir = opts.w
	if (opts.b) properties.buztoolPath = opts.b
	if (opts.c) properties.component = opts.c
	if (opts.ar) properties.artifactRepositorySettings = opts.ar
	if (opts.prop) properties.propertyFileSettings = opts.prop
	if (opts.v) properties.versionName = opts.v
	if (opts.pURL) properties.pipelineURL = opts.pURL
	if (opts.g) properties.gitBranch = opts.g
	properties.preview = (opts.p) ? 'true' : 'false'

	// validate required properties
	try {
		assert properties.buztoolPath : "Missing property buztool script path"
		assert properties.workDir: "Missing property build work directory"
		assert properties.component: "Missing property UCD component"
	} catch (AssertionError e) {
		cli.usage()
		throw e
	}
	return properties
}
