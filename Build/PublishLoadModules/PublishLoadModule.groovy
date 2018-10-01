import java.io.File
import java.io.UnsupportedEncodingException
import java.security.MessageDigest
import org.apache.http.entity.FileEntity
import com.ibm.dbb.build.*
import com.ibm.dbb.build.DBBConstants.CopyMode
import com.ibm.dbb.build.report.BuildReport
import com.ibm.dbb.build.report.records.DefaultRecordFactory
import groovyx.net.http.RESTClient

/************************************************************************************
 * This script publishes the outputs generated from a build to an artifactory
 * repository. 
 *
 ************************************************************************************/

def properties = BuildProperties.getInstance()
def workDir = properties.workDir
def loadDatasets = properties.loadDatasets

//Retrieve the build report and parse the outputs from the build report
def buildReportFile = new File("$workDir/BuildReport.json")
assert buildReportFile.exists(), "$buildReportFile does not exist"

def buildReport = BuildReport.parse(buildReportFile.newInputStream())
def executes = buildReport.records.findAll { record ->
    record.type == DefaultRecordFactory.TYPE_EXECUTE && !record.outputs.isEmpty()
}

assert executes.size() > 0, "There are no outputs found in the build report"

//If the user specifies the build property 'loadDatasets' then retrieves it
//and filters out only outputs that match with the specified data sets.
def loadDatasetArray  = loadDatasets?.split(",")
def loadDatasetList = loadDatasetArray == null ? [] : Arrays.asList(loadDatasetArray)

def loadDatasetToMembersMap = [:]  
def loadCount = 0
executes.each { execute ->
    execute.outputs.each { output ->
        def (dataset, member) = output.dataset.split("\\(|\\)")        
        if (loadDatasetList.isEmpty() || loadDatasetList.contains(dataset))
        {            
            if (loadDatasetToMembersMap[dataset] == null)
                loadDatasetToMembersMap[dataset] = []
            loadDatasetToMembersMap[dataset].add(member)
            loadCount++        
        }
    }
}

assert loadCount > 0, "There are no load modules to publish"

//Create a temporary directory on zFS to copy the load modules from data sets to
def tempLoadDir = new File("$workDir/tempLoadDir")
!tempLoadDir.exists() ?: tempLoadDir.deleteDir()
tempLoadDir.mkdirs()

//For each load modules, use CopyToHFS with option 'CopyMode.LOAD' to maintain
//SSI and 
CopyToHFS copy = new CopyToHFS().copyMode(CopyMode.LOAD)
println "Number of load modules to publish: $loadCount"
loadDatasetToMembersMap.each { dataset, members ->
    members.each { member ->
        def fullyQualifiedDsn = "$dataset($member)"
        def file = new File(tempLoadDir, member)
        copy.dataset(dataset).member(member).file(file).copy()
        println "Copying $dataset($member) to $tempLoadDir"
    }
}

//Package the load files just copied into a tar file using the build
//label as the name for the tar file.
def buildGroup = "${properties.collection}" as String
def buildLabel = "build.${properties.startTime}" as String
def tarFile = new File("$tempLoadDir/${buildLabel}.tar")
def process = "tar -cvf $tarFile .".execute(null, tempLoadDir)
int rc = process.waitFor()
assert rc == 0, "Failed to package load modules" 

//Set up the artifactory information to publish the tar file
def url = properties.get('artifactory.url')
def apiKey = properties.get('artifactory.apiKey')
def repo = properties.get('artifactory.repo') as String
def remotePath = "${buildGroup}/${tarFile.name}"

//Call the ArtifactoryHelpers to publish the tar file
File artifactoryHelpersFile = new File('./ArtifactoryHelpers.groovy')
Class artifactoryHelpersClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(artifactoryHelpersFile)
GroovyObject artifactoryHelpers = (GroovyObject) artifactoryHelpersClass.newInstance()
artifactoryHelpers.publish(url, repo, apiKey, remotePath, tarFile)





