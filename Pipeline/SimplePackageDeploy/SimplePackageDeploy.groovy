@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import java.io.File
import com.ibm.dbb.build.*
import com.ibm.dbb.build.DBBConstants.CopyMode
import com.ibm.dbb.build.report.BuildReport
import com.ibm.dbb.build.report.records.DefaultRecordFactory
import groovy.transform.*
import groovy.cli.commons.*
import groovy.json.JsonSlurper

/************************************************************************************
 * NAME: SimplePackageDeploy.groovy
 * 
 * DESCRIPTION: Deploy the package created by PackageBuildOutputs.groovy to target 
 * libraries
 *  
 * This is a simple deployment script to untar the deployment package created by 
 * PackageBuildOutputs.groovy, read the BuildReport.json and copy the components
 * to the respective environment target libraries.
 * 
 * As of now, this script does not perform any activation activities like a CICS 
 * NEWCOPY or a DB2 BIND.  
 * 
 * INVOCATION: groovyz SimplePackageDeploy.groovy [parameters]
 *
 * PARAMETERS:
 *
 * --workDir <dir>             Absolute path to the package untar temporary folder
 * --tarFileName <filename>    Name of the package tar file with path
 * --hlq <hlq>                 HLQ of the target environment libraries for the deployment 
 * --packageWithExtension      Flag to show the package contains extension (Optional) 
 * 
 * SimplePackageDeploy.properties:
 * copyModeMap                 JSON string of deploy type and copy mode pairs
 * targetLibLLQMap             JSON string of deploy type and target LLQ pairs 
 *
 * INTERNALS: 
 *
 *  1) Reads the options --workDir, --tarFileName, and --hlq from the command line. Pass the optional 
 *     parm --packageWithExtension if the build package is created with addExtension option. 
 *  2) Reads the options copyModeMap and targetLibLLQMap from the SimplePackageDeploy.properties 
 *     file.
 *  3) Creates a temporary package extract folder in the path provided using the 
 *     --workDir option.
 *  4) Untars the deployment package specified by --tarFileName to the temporary 
 *     folder created in the previous step.
 *  5) Reads the BuildReport.json file from the temporary folder.
 *  6) Identifies all the components to be deployed from the BuildReport.json file.
 *  7) Identifies deploy types for the components to be deployed from the 
 *     BuildReport.json file.
 *  8) Identifies the target libraries based on the deploy type by concatenating the
 *     --hlq value with the correspoding LLQ value from the JSON string - targetLibLLQMap.
 *  9) Identifies the copy mode based on the deploy type and corresponding copy mode 
 *     from the JSON string - copyModeMap.
 * 10) Copies the components using the copy mode to the respective target libraries.
 * 11) Deletes the temporary package extract folder after deployment.
 *
 * RETURNS:
 *    Exit Status 0       -  Success
 *    Exit Status 1       -  Failure
 *
 ************************************************************************************/

@Field Properties props = null
props = parseInput(args)

def startTime = new Date()
props.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** SimplePackageDeploy start at $props.startTime")
println("** Properties at startup:")
props.each{k,v->
	println "     $k -> $v"
}

// Map of last level dataset qualifier to DBB CopyToPDS CopyMode.
def copyModeMap = parseJSONStringToMap(props.copyModeMap)
// Map of last level dataset qualifier to Target PDS last level qualifier.
def targetLibLLQMap = parseJSONStringToMap(props.targetLibLLQMap)

// Define and create tar file extract directory 
def tarExtractDirName = "${props.workDir}/DeployFiles_${props.startTime}"
def tarExtractDir = new File("${tarExtractDirName}")

!tarExtractDir.exists() ?: tarExtractDir.deleteDir()
tarExtractDir.mkdirs()
println("\n** Created tar file extract directory $tarExtractDirName")       

// Check if the tar file provided is existing 
def tarFile = new File("${props.tarFileName}")

if(!tarFile.exists()){
    println("** Tar file not found at $tarFile")
    System.exit(1)
}


// Untar the tar file
println("** Untarring file $tarFile to $tarExtractDirName.")

def processCmd = ["sh", "-c", "tar -C $tarExtractDirName -xvf $tarFile"]

def rc = runProcess(processCmd)
if (rc != 0) {
	println("Failed to untar $tarFile")
	System.exit(1)
}


// Check if BuildReport.json exists, deploy files if exists and if not stop deploy 

def buildReportFile = new File("${tarExtractDirName}/BuildReport.json")
if (buildReportFile.exists()) {	
	deployFromBuildReport(buildReportFile,tarExtractDirName,targetLibLLQMap,copyModeMap)
	cleanTempFolder(tarExtractDir)
} else {
	println("*! Build report data at $tarExtractDirName/BuildReport.json not found")
	println("*! Deployment stopped")
	cleanTempFolder(tarExtractDir)
	System.exit(1)
}

/**********************************************************************************
 **** Deploy from the BuildReport                  
 **********************************************************************************/
def deployFromBuildReport (File buildReportFile, String tarExtractDirName,Map targetLibLLQMap,Map copyModeMap) {
	
	println("** Deploying the contents in ${buildReportFile}")
	
	def buildReport= BuildReport.parse(new FileInputStream(buildReportFile))
	def tarExtractDir = new File("${tarExtractDirName}")

	//finds all the build outputs with a deployType
	def executes= buildReport.getRecords().findAll{
	 try {
	     (it.getType()==DefaultRecordFactory.TYPE_EXECUTE &&
	             !it.getOutputs().isEmpty())
	 } catch (Exception e){}
	}


	//New CopyToPDS instance 
	CopyToPDS copy = new CopyToPDS()

	//Copy the extracted files to the corresponding target PDS
	executes.each { 
	 it.getOutputs().each {
	     if (it.deployType != null) {
	         def (dataset, member) = it.dataset.split("\\(|\\)")
	         def srcFilePath = tarExtractDirName + "/" + dataset + "/" + member		
	         if (props.packageWithExtension && props.packageWithExtension.toBoolean()) {	
	         	srcFilePath = srcFilePath + ".${it.deployType}"
	         }
	         
	         def targetPDS = "${props.hlq}" + "." + targetLibLLQMap["${it.deployType}"]
	      
	         if (copyModeMap["${it.deployType}"] != null) {
	             copy.setCopyMode(DBBConstants.CopyMode.valueOf(copyModeMap["${it.deployType}"]))
	         
	             def srcFile = new File("$srcFilePath")
	             if(!srcFile.exists()){
	                 println("*! Source file not found at $srcFilePath")
	                 println("*! Validate if the package was generated with addExtension option. If yes, use option: -e or --packageWithExtension to deploy")
	                 System.exit(1)
	             }
	             copy.setFile(srcFile);

	             copy.setDataset("$targetPDS");
	             copy.setMember("$member");
	             copy.copy()
	         
	             println("     Copied file $dataset/$member to target library $targetPDS using DBB CopyMode ${copyModeMap["${it.deployType}"]}")
	         } else {
	             println("     !ERROR: DEPLOYMENT FAILED")
	             println("     !ERROR: SOURCE FILE NOT DEPLOYED : $dataset/$member")
	             println("     !ERROR: DBB COPY MODE NOT DEFINED FOR DEPLOY TYPE : ${it.deployType}")
	             cleanTempFolder(tarExtractDir)
	             System.exit(1)
	         }
	     }    
	 }
  }	
}


/**********************************************************************************
 **** read cliArgs                  
 **********************************************************************************/
def parseInput(String[] cliArgs){
    def cli = new CliBuilder(usage: "SimplePackageDeploy.groovy [options]")
    // required packaging options
    cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the package untar temporary folder')
    cli.t(longOpt:'tarFileName', args:1, argName:'filename', 'Name of the package tar file with path')
    cli.q(longOpt:'hlq', args:1, 'HLQ of the target environment libraries for the deployment')
    cli.e(longOpt:'packageWithExtension', 'Flag to show the package contains extension (Optional)')
    cli.h(longOpt:'help', 'Prints this message')

    def opts = cli.parse(cliArgs)
    def props = new Properties()
    if (opts.h) { // if help option used, print usage and exit
        cli.usage()
        System.exit(0)
    }   
    
    if (opts.w) props.workDir = opts.w
    if (opts.t) props.tarFileName = opts.t
    if (opts.q) props.hlq = opts.q
    props.packageWithExtension = (opts.e) ? 'true' : 'false'
         
    // read properties file
    if (opts.properties){
        def propertiesFile = new File(opts.properties)
        if (propertiesFile.exists()){
            propertiesFile.withInputStream { props.load(it) }
        }
    } else { // read default properties file shipped with the script
        def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
        def defaultPackagePropFile = new File("$scriptDir/SimplePackageDeploy.properties")
        if (defaultPackagePropFile.exists()){
            defaultPackagePropFile.withInputStream { props.load(it) }
        }
    }
    
    // validate required props
    try {
        assert props.workDir : "Missing property workDir"
        assert props.tarFileName : "Missing property tarFileName"
        assert props.hlq : "Missing property hlq"
        assert props.copyModeMap : "Missing property copyModeMap"
        assert props.targetLibLLQMap : "Missing property targetLibLLQMap"

    } catch (AssertionError e) {
        cli.usage()
        throw e
    }    
    return props
}

/**********************************************************************************
 * run process
 **********************************************************************************/
def runProcess(ArrayList cmd){
    StringBuffer response = new StringBuffer()
    StringBuffer error = new StringBuffer()

    // execute cmd
    def p = cmd.execute()

    p.waitForProcessOutput(response, error)
    println(response.toString())

    def rc = p.exitValue();
    if(rc!=0){
        println("*! Error executing $cmd \n" + error.toString())
    }
    return rc
}

/**********************************************************************************
 *  This is a helper method which parses a JSON String representing a map of key 
 *  value pairs to a proper map
 *  e.q. copyModeMap = {"JCL": "TEXT", "DBRM": "BINARY", "LOAD": "LOAD"}
 ***********************************************************************************/

def parseJSONStringToMap(String packageProperty) {
	Map map = [:]
	try {
		JsonSlurper slurper = new groovy.json.JsonSlurper()
		map = slurper.parseText(packageProperty)
	} catch (Exception e) {
		String errorMsg = "*! SimplePackageDeploy.parseStringToMap - Failed to parse setting $packageProperty from String into a Map object. Process exiting."
		println errorMsg
		println e.getMessage()
		System.exit(1)
	}
	return map
}

/**********************************************************************************
 *  Clean the temporary folder before exit
 **********************************************************************************/
 
def cleanTempFolder(File tarExtractDir) {
	if (tarExtractDir.exists()) tarExtractDir.deleteDir()
    println("** Cleaning up the temporary folder - ${tarExtractDir.getAbsolutePath()} \n")
}