import java.nio.file.*
import com.ibm.dbb.*
import groovy.transform.*
import groovy.cli.commons.*
import groovy.xml.*

//******************************************************************************
//* Retrieves DBB environments
//******************************************************************************
dbbHome = System.getenv("DBB_HOME")
if (!dbbHome)
{
    println "Need to specified the required environment 'DBB_HOME'"
    System.exit(1)
}
dbbConf = System.getenv("DBB_CONF")?:EnvVars.getConf()

//******************************************************************************
//* Parses and validates the input arguments
//******************************************************************************
def headerMsg = 'Extract information of a SCLM project to the temporary data sets and files on HFS'
cli = new CliBuilder(usage: 'SclmExtract sclmmig.config', header: headerMsg, stopAtNonOption: false)
cli.h(longOpt:'help', 'Prints this message')

def parameters = cli.parse(args)
if (parameters.arguments().size() < 1)
{
    cli.usage()
    System.exit(2)
}

@SourceURI
URI sourceUri
Path scriptLocation = Paths.get(sourceUri).parent

//******************************************************************************
//* Parses the SCLM migration config file
//******************************************************************************
def configFile = scriptLocation.resolve(parameters.arguments()[0]).toFile()
if (!configFile.exists())
{
    println "File $configFile does not exist. Need to specify a valid SCLM migration config file"
    System.exit(1)
}

//******************************************************************************
//* Parses the SCLM migration config file
//******************************************************************************
def config = new Properties()
config.load(configFile.newDataInputStream())

def selectionCriteria = config.selectionCriteria
def proj = config.proj
def outputDir = new File(config.outputDir ?: System.getProperty('user.home'))
outputDir = new File("$outputDir/sclmMigration/${proj.toLowerCase()}")
def repo = (config.repo ?: "$outputDir/repo") + "/${proj.toLowerCase()}"
def group = config.group
def migHlq = config.migHlq
if (migHlq == '')
{
  def me = "whoami".execute().text.trim()
  if (me == 'ROOT')
  {
      println "User running migration is UID=0 therefore userid is ROOT. Please enter a valid id in migHlq in the config file"
      System.exit(1)
  }
  migHlq = me.tokenize(" ")[0]
}
def tempHlq = "${migHlq}"
def matchHlq = "${tempHlq}(\\.${proj}\\.${group}\\.V\\d)?\\."
def matchCur = "${tempHlq}(\\.${proj}\\.${group}\\.VCUR)?\\."
def matchRepo = "$repo/"
//println "matchHlq = $matchHlq"
//println "matchCur = $matchCur"
//println "matchRepo = $matchRepo"

def buildDir = new File("$repo/build")
buildDir.exists()?:buildDir.mkdirs()


def outputFiles = []


/*
 * Generate scriptMappings.properties file.  The generated content
 * is based on "selectionCriteria".
 */
def scriptMappingsFile = new File(buildDir, 'scriptMappings.properties')
!scriptMappingsFile.exists()?:scriptMappingsFile.delete()
outputFiles << scriptMappingsFile

if (selectionCriteria == 'LANG')
{
    /*
     * When selectionCriteria is 'LANG', then the members from PDS are
     * imported to /type/lang/member.ext.
     *
     * The generated script mapping must be:
     *   scriptMapping = LANG :: type/lang/*.ext
     *
     * Generates the scriptMappings.properties based on LANG is straight-forward
     * since the langext.txt already contains:
     *    LANG:TYPE:FILE_EXT
     *
     * Note: for case that there is no TYPE, replace it with '**'
     */
    def langextFile = new File(outputDir, 'langext.txt')
    langextFile.eachLine { line ->
        (lang,type,ext) = line.split(":").collect() { it.trim().toLowerCase() }
        type = type ?: '**'
        scriptMappingsFile << "scriptMapping = ${lang.toUpperCase()} :: $type/$lang/*.$ext" << '\n'
    }
}
else if (selectionCriteria == 'TYPE')
{
    /*
     * When selectionCriteria is 'TYPE', then the members from PDS are
     * imported to /type/member.ext.
     *
     * The generated script mapping could be:
     *    scriptMapping = LANG :: type/member.ext
     *
     * We would have a scriptMapping for each member being imported, therefore
     * we have to rely on the 'memberToFile.txt' file which is generated after the
     * zimport.  We also have to rely on the 'members.txt' which contains member:lang
     * mapping.
     */
    def memberToFileMap = [:]
    def memberToFileFile = new File("$outputDir/memberToFile.txt")
    memberToFileFile.eachLine {
        if (it ==~ /.*\.V\d\..*/) {
            (dsn,file) = it.split(" ")
            dsn.eachMatch(matchHlq) { match ->
                match.each { matchedGroup ->
                    dsn = dsn - matchedGroup
                }
            }
            file.eachMatch(matchRepo) { match ->
                file = file - match
            }
            memberToFileMap."$dsn" = file
        }
        if (it ==~ /.*\.VCUR..*/) {
            (dsn,file) = it.split(" ")
            dsn.eachMatch(matchCur) { match ->
                match.each { matchedGroup ->
                    dsn = dsn - matchedGroup
                }
            }
            file.eachMatch(matchRepo) { match ->
                file = file - match
            }
            memberToFileMap."$dsn" = file
        }
    }

    //println memberToFileMap

    def langToFilesMap = [:]
    def membersFile =  new File("$outputDir/members.txt")
    membersFile.eachLine {
        if (it ==~ /.*\.V\d\..*/) {
            (dsn,lang) = it.split(" ")
            def files = langToFilesMap."$lang"
            if (!files)
            {
                files = [] as Set
                langToFilesMap."$lang" = files
            }
            dsn.eachMatch(matchHlq) { match ->
                match.each { matchedGroup ->
                    dsn = dsn - matchedGroup
                }
            }
            def file = memberToFileMap."$dsn"
            files << file
        }
        if (it ==~ /.*\.VCUR..*/) {
            (dsn,lang) = it.split(" ")
            def files = langToFilesMap."$lang"
            if (!files)
            {
                files = [] as Set
                langToFilesMap."$lang" = files
            }
            dsn.eachMatch(matchCur) { match ->
                match.each { matchedGroup ->
                    dsn = dsn - matchedGroup
                }
            }
            def file = memberToFileMap."$dsn"
            files << file
        }
    }

    //println langToFilesMap

    langToFilesMap.each { entry ->
        def lang = entry.key
        def paths = entry.value
        //println "==> $lang - $paths"
        scriptMappingsFile << "scriptMapping = $lang :: ${paths.join(',\\\n      ')}" << '\n'
    }
}
else
{
    println "Cannot generate script mappings because selectionCriteria is invalid '$selectionCriteria"
    System.exit(1)
}

/**
 * Generate datasetMappings.txt file.  This file contain the mapping
 * of PDS to a directory, for example:
 *
 * datasetMapping = COPYBOOK :: copybook
 *
 * We have to go through each entry in memberToFile.txt and strip off
 * the HLQ from the data set and strip off the Git repository directory
 * from the file location and the file name.  Then create a mapping
 * of dataset to directory.
 */
def datasetToFolderMap = [:]
def fileList = [] as Set
def memberToFileFile = new File("$outputDir/memberToFile.txt")
memberToFileFile.eachLine {
    if (it ==~ /.*\.V\d\..*/) {
        (dsn,file) = it.split(" ")
        dsn.eachMatch(matchHlq) { match ->
            match.each { matchedGroup ->
                dsn = dsn - matchedGroup
            }
        }
        def dataset = dsn.take(dsn.indexOf('('))
        file.eachMatch(matchRepo) { match ->
            file = file - match
            fileList << file
        }
        def folder = file.take(file.lastIndexOf('/'))
        def folders = datasetToFolderMap."$dataset"
        if (!folders)
        {
            folders = [] as Set
            datasetToFolderMap."$dataset" = folders
        }
        folders << folder
    }
    if (it ==~ /.*\.VCUR..*/) {
        (dsn,file) = it.split(" ")
        dsn.eachMatch(matchCur) { match ->
            match.each { matchedGroup ->
                dsn = dsn - matchedGroup
            }
        }
        def dataset = dsn.take(dsn.indexOf('('))
        file.eachMatch(matchRepo) { match ->
            file = file - match
            fileList << file
        }
        def folder = file.take(file.lastIndexOf('/'))
        def folders = datasetToFolderMap."$dataset"
        if (!folders)
        {
            folders = [] as Set
            datasetToFolderMap."$dataset" = folders
        }
        folders << folder
    }
}

def datasetMappingsFile = new File(buildDir, 'datasetMappings.properties')
!datasetMappingsFile.exists()?:datasetMappingsFile.delete()
outputFiles << datasetMappingsFile

datasetToFolderMap.each {
    def dataset = it.key
    def folders = it.value
    datasetMappingsFile << "datasetMapping = $dataset :: ${folders.join(',\\\n       ')}" << '\n'
}

/*
 * Generate the files.txt file.  This contains the relative path
 * of all files being imported.  Again, the information is from
 * memberToFile.txt
 */
def files = new File(buildDir, 'files.txt')
!files.exists()?:files.delete()
outputFiles << files

files << fileList.join('\n')

/*
 * Generate files.properties file. The content is based on
 * the file overrides specified in fileMetaData.xml file.
 *
 * Note: the fileMetaData.xml is using regular expression, but
 * the pattern is basically a path that ends with a specific
 * file name.  For that reason, during the conversion, we
 * simply use the glob pattern which should be faster and easy
 * to read.
 */
def metadataFile = new File(outputDir, 'fileMetaData.xml')
def parser = new XmlSlurper().parse(metadataFile)
def fileMetadataRules = parser."**".findAll { node ->
    node.name() == 'filemetadatarule' && !node.@match.text().isEmpty() && node.@name.toString().startsWith('team.enterprise.build.var.')
}
def fileProperties = []
fileMetadataRules.each { rule ->
    def key = rule.@name.toString() - 'team.enterprise.build.var.'
    def value = rule.@value
    def wildcard = (selectionCriteria == 'LANG' ? '**/' : '*/')
    def pattern = wildcard + rule.@match.toString().drop(3).minus('\\').minus('$').toLowerCase()
    fileProperties << "$key = $value :: $pattern"
}

def propertyFile = new File(buildDir, 'files.properties')
!propertyFile.exists()?:propertyFile.delete()
outputFiles << propertyFile

propertyFile << fileProperties.join('\n')


/*
 * Generate build.properties file.
 */
def buildPropertyFile = new File(buildDir, 'build.properties')
!buildPropertyFile.exists()?:buildPropertyFile.delete()
outputFiles << buildPropertyFile

buildPropertyFile << "# The directory of the local Git repository" << '\n'
buildPropertyFile << "sourceDir=$repo" << '\n\n'
buildPropertyFile << "# Absolute path to the build output directory" << '\n'
buildPropertyFile << 'workDir=${sourceDir}/work' << '\n\n'
buildPropertyFile << "# High Level Qualifier for build data sets" << '\n'
buildPropertyFile << "HLQ=${tempHlq}.${proj}" << '\n\n'
buildPropertyFile << "# DBB Repository Web Application authentication properties" << '\n'
buildPropertyFile << "dbb.RepositoryClient.url=" << '\n'
buildPropertyFile << "dbb.RepositoryClient.userId=" << '\n'
buildPropertyFile << "dbb.RepositoryClient.passwordFile=" << '\n\n'
buildPropertyFile << "####################################################################" << '\n'
buildPropertyFile << "### The following properties can use the default values provided ###" << '\n'
buildPropertyFile << "####################################################################" << '\n\n'
buildPropertyFile << "# DBB Repository Dependency Data Collection Name" << '\n'
buildPropertyFile << "collection=${proj}" << '\n\n'
buildPropertyFile << "# Option for conditional build" << '\n'
buildPropertyFile << "failOnError=true" << '\n'

/*
 * Output the results
 */
println "There are ${outputFiles.size()} files generated in directory $buildDir:"
outputFiles.toSorted().collect { it.name }.each {
    println "   $it"
}
