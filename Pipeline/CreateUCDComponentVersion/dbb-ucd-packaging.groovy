import com.ibm.dbb.build.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.time.*
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
 *  -ar,--artifactRepository	  Absolute path to Artifact Respository Server connection file (Optional)
 *  -p,--preview				  Preview, not executing buztool.sh
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
	System.exit()
}

def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))

// parse build report to find the build result meta info
def buildResult = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT}[0];
def dependencies = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET};

// parse build report to find the build outputs to be deployed.
println("** Find deployable outputs in the build report ")

// the following example finds all the build outputs with and without deployType
//def executes= buildReport.getRecords().findAll{
//	it.getType()==DefaultRecordFactory.TYPE_EXECUTE
//}

// Print warning, that extraction of COPY_TO_PDS records are not supported with older versions of the dbb toolkit. 
dbbVersion = new VersionInfo().getVersion()
println "   ! Buildrecord type TYPE_COPY_TO_PDS is supported with DBB toolkit 1.0.8 and higher. Identified DBB Toolkit version $dbbVersion. Extracting build records for TYPE_COPY_TO_PDS will be skipped."

// finds all the build outputs with a deployType
def executes= buildReport.getRecords().findAll{
	try {
		(it.getType()==DefaultRecordFactory.TYPE_EXECUTE || it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS) &&
				!it.getOutputs().findAll{ o ->
					o.deployType != null
				}.isEmpty()
	} catch (Exception e){}	
}

executes.each { it.getOutputs().each { println("   ${it.dataset}, ${it.deployType}")}}

// generate ship list file. specification of UCD ship list can be found at
// https://www.ibm.com/support/knowledgecenter/SS4GSP_6.2.7/com.ibm.udeploy.doc/topics/zos_shiplistfiles.html
println("** Generate UCD ship list file")
def writer = new StringWriter()
writer.write("<?xml version=\"1.0\" encoding=\"CP037\"?>\n");
def xml = new MarkupBuilder(writer)
xml.manifest(type:"MANIFEST_SHIPLIST"){
	//top level property will be added as version properties
	//requires UCD v6.2.6 and above
	property(name : buildResult.getGroup() + "-buildResultUrl", value : buildResult.getUrl())
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

					// add source information
					inputs(url : ""){
						input(name : execute.getFile(), compileType : "Main")
						dependencies.each{
							if(it.getId() == execute.getFile()){
								it.getAllDependencies().each{
									def displayName = it.getFile()? it.getFile() : it.getLname()
									input(name : displayName, compileType : it.getCategory())
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
// set aritifactRepository option if specified
if (properties.artifactRepositorySettings) {
	cmd << "-ar"
	cmd << properties.artifactRepositorySettings
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
	def p = cmd.execute();
	p.waitFor();
	println(p.text);

	def rc = p.exitValue();
	if(rc==0){
		println("** buztool output properties")
		def outputProp = new Properties()
		new File("${properties.workDir}/buztool.output").withInputStream { outputProp.load(it) }
		outputProp.each{k,v->
			println "   $k -> $v"
		}
	}else{
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
	cli.v(longOpt:'versionName', args:1, argName:'versionName', 'Name of the UCD component version')
	cli.p(longOpt:'preview', 'Preview mode - generate shiplist, but do not run buztool.sh')
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

	// set command line arguments
	if (opts.w) properties.workDir = opts.w
	if (opts.b) properties.buztoolPath = opts.b
	if (opts.c) properties.component = opts.c
	if (opts.ar) properties.artifactRepositorySettings = opts.ar
	if (opts.v) properties.versionName = opts.v
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
