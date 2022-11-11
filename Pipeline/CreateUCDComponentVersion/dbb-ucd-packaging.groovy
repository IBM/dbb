import com.ibm.dbb.build.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.time.*
import groovy.cli.commons.*
import com.ibm.dbb.build.VersionInfo
import groovy.xml.MarkupBuilder
import groovy.json.JsonParserType
import groovy.json.JsonBuilder
import groovy.json.JsonSlurper
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
 *  -pURL,--pipelineURL           URL to the pipeline build result (Optional)
 *  -g,--gitBranch                Name of the git branch (Optional)
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
 *  
 * Version 7 - 2022-06 
 *  Added functionality for --buildReportOrder CLI, allowing multiple build reports to be processed at once to build cumulative packages
 *  Support for UCD packaging format v2 
 *  Ability to package deletions (requires DBB Toolkit 1.1.3 and zAppBuild 2.4.0)
 *  
 */

def properties = parseInput(args)
def startTime = new Date()
properties.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** Create version start at $properties.startTime")
println("** Properties at startup:")
properties.each{k,v->
	println "   $k -> $v"
}

/*
 * HashMap to understand and *merge* the different build outputs from the various buildreports
 *  
 <Dataset, {buildReport:Executes}> 
 dataset as key, then buildReport and Executes as a key-value pair as value 
 */
Map<String, Map> buildOutputsMap = new HashMap<String, Map>()

dbbVersion = new VersionInfo().getVersion()
println "* Buildrecord type TYPE_COPY_TO_PDS is supported with DBB toolkit 1.0.8 and higher. Extracting build records for TYPE_COPY_TO_PDS might not be available and skipped. Identified DBB Toolkit version $dbbVersion."


println("**  Reading provided build report(s).")

properties.buildReportOrder.each{ buildReportFile ->
	println("*** Parsing DBB build report $buildReportFile.")

	def buildReport = BuildReport.parse(new FileInputStream(buildReportFile))

	def executes = buildReport.getRecords().findAll{
		try {
			(it.getType()==DefaultRecordFactory.TYPE_EXECUTE || it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS) &&
					!it.getOutputs().isEmpty()
		} catch (Exception e) {}
	}

	//removes all outputs of deploytype ZUNIT-TESTCASE or null
	executes.each {
		def unwantedOutputs = it.getOutputs().findAll{ o ->
			o.deployType == null || o.deployType == 'ZUNIT-TESTCASE'
		}
		it.getOutputs().removeAll(unwantedOutputs)
	}


	def deletions = buildReport.getRecords().findAll{
		try {
			// Obtain delete records, which got added by zAppBuild
			it.getType()=="DELETE_RECORD"
		} catch (Exception e){
			println e
		}
	}

	def count = 0
	def deletionCount = 0

	// store build output information in Hashmap buildOutputsMap to replace potential duplicates
	// managing TYPE EXECUTE and COPY_TO_PDS
	executes.each{ executeRecord ->
		if(executeRecord.getOutputs().isEmpty() != true) {
			count += executeRecord.getOutputs().size()
			executeRecord.getOutputs().each{ output ->
				buildOutputsMap.put(output.dataset, [buildReport, executeRecord])
			}
		}
	}

	// store build output information in Hashmap buildOutputsMap to replace potential duplicates
	// managing DELETE_RECORD leveraging the generic AnyTypeRecord Type
	deletions.each { deleteRecord ->
		deletionCount += deleteRecord.getAttributeAsList("deletedBuildOutputs").size()
		deleteRecord.getAttributeAsList("deletedBuildOutputs").each{ deletedFile ->
			buildOutputsMap.put(deletedFile, [buildReport, deleteRecord])
		}

	}

	if ( count + deletionCount == 0 ) {
		println("**  No items to package in $buildReportFile.")
	} else {
		// Log deployable files
		if (count != 0) {
			println("**  Deployable files detected in $buildReportFile")
			executes.each { it.getOutputs().each { println("   ${it.dataset}, ${it.deployType}")}}
		}

		// Log deleted files
		if (deletionCount != 0) {
			println("**  Deleted files detected in $buildReportFile")
			deletions.each { it.getAttributeAsList("deletedBuildOutputs").each { println("   ${it}")}}
		}
	}
}

if (buildOutputsMap.size() == 0 ) {
	println("** No items to package in the provided build reports (s). Process exiting.")
	System.exit(2)
}


// generate ship list file. specification of UCD ship list can be found at
// https://www.ibm.com/support/knowledgecenter/SS4GSP_6.2.7/com.ibm.udeploy.doc/topics/zos_shiplistfiles.html
println("** Generate UCD ship list file")
def writer = new StringWriter()
writer.write("<?xml version=\"1.0\" encoding=\"CP037\"?>\n");
def xml = new MarkupBuilder(writer)
xml.manifest(type:"MANIFEST_SHIPLIST"){

	println "   Creating general UCD component version properties."

	// Url to CI pipeline
	if (properties.pipelineURL) property(name : "ci-pipelineUrl", value : properties.pipelineURL )
	// Git branch
	if (properties.pullRequestURL) property(name : "ci-pullRequestURL", value : properties.pullRequestURL )
	// Git branch
	if (properties.gitBranch) property(name : "ci-gitBranch", value : properties.gitBranch )

	// flag for single build report reporting
	boolean singleBuildReportReporting = true	
	
	// iterate over Hashmap to generate container entries for UCD shiplist.
	buildOutputsMap.each{ dataset, info ->
		buildReport = info[0]
		record = info[1]

		// obtain build info from the build result record
		def buildResult = buildReport.getRecords().findAll{it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT}[0]
		def buildResultRecord = buildReport.getRecords().find{
			try {
				it.getType()==DefaultRecordFactory.TYPE_PROPERTIES && it.getId()=="DBB.BuildResultProperties"
			} catch (Exception e){}
		}
		def buildResultProperties = null
		if(buildResultRecord!=null){
			buildResultProperties = buildResultRecord.getProperties()
		}

		// document DBB build properties as component version properties in case of a single build report (keep backward compatibility)
		if (properties.buildReportOrder.size() == 1 && singleBuildReportReporting) {
			println "   Storing DBB Build result properties as general component version properties due to single build report."
			
			// Url to DBB Build result
			if (buildResult != null) {
				property(name : "dbb-buildResultUrl", label: buildResult.getLabel(), value : buildResult.getUrl())
			}
			// Populate build result properties
			if (buildResultProperties != null) {
				buildResultProperties.each{
					//not all properties need to be included in the shiplist
					//can ignore files processed
					//can ignore full build / impact build
					property(name:it.key, value:it.value)
				}
			}
			
			// Shiplist entry created, no need to document it a second time 
			singleBuildReportReporting = false
		}
		
		println "   Creating shiplist record for build output $dataset with recordType $record.type."
		
		// process TYPE_EXECUTE and TYPE_COPY_TO_PDS
		if (record.getType()==DefaultRecordFactory.TYPE_EXECUTE || record.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS) {

			record.getOutputs().each{ output ->
				// process only outputs of the key of the map
				if (dataset == output.dataset) {
					def (ds, member) = getDatasetName(output.dataset)
					//println "    Defining container for $output.dataset."
					def containerAttributes = getContainerAttributes(ds, properties)
					container(containerAttributes){
						resource(name:member, type:"PDSMember", deployType:output.deployType){

							// document dbb build result url and build properties on the element level when there are more than one buildReport processed
							if (properties.buildReportOrder.size() != 1) {

								// Url to DBB Build result
								if (buildResult != null) {
									property(name : "dbb-buildResultUrl", label: buildResult.getLabel(), value : buildResult.getUrl())
								}	
								// Populate build result properties
								if (buildResultProperties != null) {
									buildResultProperties.each{
										//not all properties need to be included in the shiplist
										//can ignore files processed
										//can ignore full build / impact build
										property(name:it.key, value:it.value)
									}
								}
							}

							property(name:"buildcommand", value:record.getCommand())

							// Only TYPE_EXECUTE Records carry options
							if (record.getType()==DefaultRecordFactory.TYPE_EXECUTE) property(name:"buildoptions", value:record.getOptions())
							
							// Sample to add additional artifact properties. Here: adding db2 properties for a DBRM
							//   which where added to the build report through a basic PropertiesRecord.
							//   see https://github.com/IBM/dbb-zappbuild/blob/06ff114ee22b4e41a09aa0640ac75b7e56c70521/build-conf/build.properties#L79-L89
								
							if (output.deployType.equals("DBRM")){
								propertyRecord = buildReport.getRecords().findAll{
									it.getType()==DefaultRecordFactory.TYPE_PROPERTIES && it.getProperty("file")==record.getFile()
								}
								propertyRecord.each { propertyRec ->
									// Iterate Properties
									(propertyRec.getProperties()).each {
										property(name:"$it.key", value:it.value)
									}
								}
							}
							
							// add githash to container
							def githash = "" // set empty
							if (buildResultProperties != null){
								// get git references from build properties
								def gitproperty = buildResultProperties.find{
									it.key.contains(":githash:") && record.getFile().contains(it.key.substring(9))
								}
								if (gitproperty != null ) {
									githash = gitproperty.getValue()
									// set properties in shiplist
									property(name:"githash", value:githash)
									if(properties.git_commitURL_prefix) property(name:"git-link-to-commit", value:"${properties.git_commitURL_prefix}/${githash}")
								}
							}
							
							// add source information in the input column of UCD
							inputUrl = (buildResultProperties != null && properties.git_treeURL_prefix && githash!="") ? "${properties.git_treeURL_prefix}/${githash}/"+ record.getFile() : ""
							inputs(url : "${inputUrl}"){
								input(name : record.getFile(), compileType : "Main", url : inputUrl)
								
								// adding dependencies
								def dependencySets = buildReport.getRecords().findAll{
									it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET && it.getFile()==record.getFile()
								};
								Set<String> dependencyCache = new HashSet<String>()
								dependencySets.unique().each{
									it.getAllDependencies().each{
										if (it.isResolved() && !dependencyCache.contains(it.getLname()) && it.getFile()!=record.getFile()){
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
		else if (record.getType()=="DELETE_RECORD") {
			// document delete container
			
			deletedFiles = record.getAttributeAsList("deletedBuildOutputs")
			deletedFiles.each { deletedOutput ->

				// process only outputs of the key of the map
				if(dataset == deletedOutput) {

					def (ds,member) = getDatasetName(deletedOutput)
					println "   Defining shiplist delete container for $deletedOutput."

					deleted{
						// create container
						def containerAttributes = getContainerAttributes(ds, properties)
						container(containerAttributes){
							resource(name:member, type:"PDSMember")

							// document dbb build result url and build properties on the element level when there are more than one buildReport processed
							if (properties.buildReportOrder.size() != 1) {

								// Url to DBB Build result
								property(name : "dbb-buildResultUrl", label: buildResult.getLabel(), value : buildResult.getUrl())
								// Populate build result properties
								if (buildResultProperties != null) {
									buildResultProperties.each{
										//not all properties need to be included in the shiplist
										//can ignore files processed
										//can ignore full build / impact build
										property(name:it.key, value:it.value)
									}
								}
							}
							
							// add githash to container
							def githash = "" // set empty
							if (buildResultProperties != null){
								// get git references from build properties
								def gitproperty = buildResultProperties.find{
									it.key.contains(":githash:") && record.getAttribute("file").contains(it.key.substring(9))
								}
								if (gitproperty != null ) {
									githash = gitproperty.getValue()
									// set properties in shiplist
									property(name:"githash", value:githash)
									if(properties.git_commitURL_prefix) property(name:"git-link-to-commit", value:"${properties.git_commitURL_prefix}/${githash}")
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

// pass buztool package version2 if specified
// https://www.ibm.com/docs/en/urbancode-deploy/7.2.1?topic=czcv-creating-zos-component-version-using-v2-package-format
def buztoolOption = "createzosversion"
if (properties.ucdV2PackageFormat.toBoolean()) {
	buztoolOption = "createzosversion2"
}

def cmd = [
	properties.buztoolPath,
	buztoolOption,
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

// set buztoolPropertyFile option if specified
if (properties.buztoolPropertyFile) {
	cmd << "-prop"
	cmd << properties.buztoolPropertyFile
}

//set component version name if specified
if(properties.versionName){
	cmd << "-v"
	cmd <<  "\"${properties.versionName}\""
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

/**
 */

/**
 * calcualte the container attributes for default package or package v2 
 */
// define container attributes
def getContainerAttributes(String ds, Properties properties) {
	def containerAttMap = [:]
	if (properties.ucdV2PackageFormat.toBoolean()) {
		// ucd package format v2 requres to set a deployType at container level
		def containerDeployType
		def lastLevelQual = ds.tokenize('.').last()
		if (properties.containerMapping) {
			// obtain the deployType setting from the property
			def cMapping = parseJSONStringToMap(properties.containerMapping)
			containerDeployType = cMapping[lastLevelQual]
			if (containerDeployType == null) {
				println "*!* UCD Packaging v2 formar requires a mapping for the copymode for $lastLevelQual through the containerMapping property - see $properties.containerMapping."
			}
		} else {
			// set the last level qualifier as deployType
			containerDeployType = lastLevelQual
		}
		// create container element with deployType
		containerAttMap = [name:ds, type:"PDS", deployType:containerDeployType]
	}else {
		// create container without deployType attribute
		containerAttMap = [name:ds, type:"PDS"]
	}
	return containerAttMap
}

def parseInput(String[] cliArgs){
	def cli = new CliBuilder(usage: "deploy.groovy [options]")
	cli.b(longOpt:'buztool', args:1, argName:'file', 'Absolute path to UrbanCode Deploy buztool.sh script')
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the DBB build output directory')
	cli.c(longOpt:'component', args:1, argName:'name', 'Name of the UCD component to create version in')
	cli.ar(longOpt:'artifactRepository', args:1, argName:'artifactRepositorySettings', 'Absolute path to Artifactory Server connection file (** Deprecated, please use --propertyFile instead **)')
	cli.prop(longOpt:'propertyFile', args:1, argName:'buztoolPropertyFile', 'Absolute path to UCD buztool property file (Optional). From UCD v7.1.x and greater it replaces the -ar option')
	cli.v(longOpt:'versionName', args:1, argName:'versionName', 'Name of the UCD component version')
	cli.zpv2(longOpt:'ucdV2PackageFormat', 'Invoke buztool with the buztool package version v2.')
	cli.p(longOpt:'preview', 'Preview mode - generate shiplist, but do not run buztool.sh')

	cli.bO(longOpt:'buildReportOrder', args:1, argName:'buildReportOrder', 'Build a cumulative package based on a comma separated list of one or multiple DBB build reports processed in the provided order (Optional).')
	cli.boFile(longOpt:'buildReportOrderFile', args:1, argName:'buildReportOrderFile', 'Build a cumulative package based on an input file that lists one or multiple build reports defining the order of processing (Optional).')

	cli.ppf(longOpt:'packagingPropFiles', args:1,'Comma separated list of property files to configure the dbb-ucd-packaging script (Optional)')
	cli.rpFile(longOpt:'repositoryInfoPropertiesFile', args:1,'Absolute path to property file containing URL prefixes to git provider (Optional) (** Deprecated, please use --packagingPropFiles instead **)')
	
	// additional references to build and workflow
	cli.pURL(longOpt:'pipelineURL', args:1,'URL to the pipeline build result (Optional)')
	cli.g(longOpt:'gitBranch', args:1,'Name of the git branch (Optional)')
	cli.prURL(longOpt:'pullRequestURL', args:1,'URL to the Pull/Merge request (Optional)')

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

	// load configuration files
	if (opts.ppf){
		opts.ppf.split(",").each { propertyFile ->
			def repositoryPropFile = new File(propertyFile)
			if (repositoryPropFile.exists()){
				repositoryPropFile.withInputStream {  properties.load(it) }
			}
		}
	}

	// set command line arguments
	if (opts.w) properties.workDir = opts.w
	if (opts.b) properties.buztoolPath = opts.b
	if (opts.c) properties.component = opts.c
	if (opts.ar) properties.artifactRepositorySettings = opts.ar
	if (opts.prop) properties.buztoolPropertyFile = opts.prop
	if (opts.v) properties.versionName = opts.v
	if (opts.pURL) properties.pipelineURL = opts.pURL
	if (opts.g) properties.gitBranch = opts.g
	if (opts.prURL) properties.pullRequestURL = opts.prURL
	properties.preview = (opts.p) ? 'true' : 'false'
	properties.ucdV2PackageFormat = (opts.zpv2) ? 'true' : 'false'

	// setup single or multiple build reports
	def buildReports = []
	if (opts.boFile) {
		new File (opts.boFile).eachLine { line ->
			buildReports.add(line)
		}
	}
	
	if (opts.bO) {
		opts.bO.split(',').each{
			buildReports.add(it)
		}
	} 
	
	if (!opts.boFile && !opts.bO){ // default lookup in Workdir
		buildReports.add(opts.w + "/BuildReport.json")
	}

	properties.buildReportOrder = buildReports


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


/*
 *  This is a helper method which parses a JSON String representing a map of key value pairs to a proper map
 *  e.q. cobol_dependenciesAlternativeLibraryNameMapping = {"MYFILE": "cobol_myfilePDS", "DCLGEN" : "cobol_dclgenPDS"}
 */

def parseJSONStringToMap(String packageProperty) {
	Map map = [:]
	try {
		JsonSlurper slurper = new groovy.json.JsonSlurper()
		map = slurper.parseText(packageProperty)
	} catch (Exception e) {
		errorMsg = "*! dbb-ucd-packaging.parseStringToMap - Failed to parse setting $packageProperty from String into a Map object. Process exiting."
		println errorMsg
		println e.getMessage()
		System.exit(3)
	}
	return map
}
