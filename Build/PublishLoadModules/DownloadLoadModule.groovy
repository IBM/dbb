import com.ibm.dbb.build.*
import com.ibm.dbb.build.DBBConstants.CopyMode
 
/************************************************************************************
 * This script downloads the load modules stored in a tar file from an artifactory
 * repository to zFS.
 *
 ************************************************************************************/

def properties = parseInput(args)
def url = properties.get('artifactory.url')
def apiKey = properties.get('artifactory.apiKey')
def repo = properties.get('artifactory.repo') as String
def remotePath = properties.remotePath

//Create a temporary directory on zFS to download the load module tar file
//and unpack the tar file to.
def tempLoadDir = new File('tempLoadDir')
!tempLoadDir.exists() ?: tempLoadDir.deleteDir()
tempLoadDir.mkdirs()

//Retrieve the latest published tar file for a given Collection
File artifactoryHelpersFile = new File('./ArtifactoryHelpers.groovy')
Class artifactoryHelpersClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(artifactoryHelpersFile)
GroovyObject artifactoryHelpers = (GroovyObject) artifactoryHelpersClass.newInstance()
def latestLoadModuleTar = artifactoryHelpers.getLatestPublished(url, repo, apiKey, remotePath)

assert latestLoadModuleTar, "There is no artifacts found in $remotePath"

//Download the tar file from artifactory repository
def tarFile = new File("$tempLoadDir/$latestLoadModuleTar")
artifactoryHelpers.download(url, repo, apiKey, "$remotePath/$latestLoadModuleTar", tarFile)

//Unpack the tar file
def process = "tar -xf ${tarFile.name} .".execute(null, tempLoadDir)
int rc = process.waitFor()
assert rc == 0, "Failed to unpack load module tar file $tarFile"

//Copy all load files from zFS to the existing data set.
def dataset = properties.dataset
CopyToPDS copy = new CopyToPDS().copyMode(CopyMode.LOAD)
tempLoadDir.eachFile { file ->
    if (!file.name.endsWith('.tar'))
    {
        copy.file(file).dataset(dataset).member(file.name).copy()
        println "Copying $file to $dataset"
    }
}


//Parsing the data set from the command line and other artifactory connection information from
//the build.properties 
def parseInput(String[] cliArgs)
{
    def cli = new CliBuilder(usage: "download.groovy <dataset>", header: '<dataset> must be an existing data set', stopAtNonOption: false)       
    cli.h(longOpt:'help', 'Prints this message')
    def opts = cli.parse(cliArgs)
    if (opts.h)
    {
        cli.usage()
        System.exit(0)
    }
    
    if (opts.arguments().size == 0)
    {
        println "Need to specify an existing data set to restore the load modules to"
        cli.usage()
        System.exit(2)
    }
        
    def properties = BuildProperties.getInstance()
    properties.load(new File('./build.properties'))
    properties.remotePath = properties.collection
    properties.dataset = opts.arguments()[0]
    
    // validate required properties
    try
    {
        assert properties['artifactory.url'], 'Missing artifactory.url property'
        assert properties['artifactory.repo'], 'Missing artifactory repository'
        assert properties['artifactory.apiKey'], 'Missing artifactory apiKey'
        assert properties.dataset, 'Missing data set'
    }
    catch (AssertionError e)
    {
        println e.message
        cli.usage()
        System.exit(2)
    }
    return properties
}

