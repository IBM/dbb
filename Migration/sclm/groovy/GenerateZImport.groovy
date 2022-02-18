import com.ibm.dbb.*
import java.nio.file.*
import java.nio.file.attribute.*
import groovy.transform.*
import groovy.cli.commons.*
import com.ibm.jzos.*

/*******************************************************************************
 *
 * This script uses the information extracted from SCLM and generates a
 * migration script to migrate members stored in temporary data sets
 * extracted from SCLM in previous step.
 *
 * The input to this script is a migrationInfo.txt generated from previous
 * step (SclmExtract.groovy).
 *
 * This script also generates a member to file mapping.  This is useful
 * for generate DBB script mapping.
 *
 *******************************************************************************/

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

def headerMsg = 'Use this script to generate migration script to migrate source members from data sets to a local GIT repository'
cli = new CliBuilder(usage: 'migrate <migrationInfo.txt>', header: headerMsg, stopAtNonOption: false)

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
config = new Properties()
config.load(configFile.newDataInputStream())

proj = config.proj
group = config.group
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
tempHlq = "${migHlq}"
def outputDir = new File(config.outputDir ?: System.getProperty('user.home'))
workDir = "$outputDir/sclmMigration/${proj.toLowerCase()}"
selectionCriteria = config.selectionCriteria

//******************************************************************************
//* The generated migration script file
//******************************************************************************
migrationScriptFile = new File("$workDir/zimport.sh")
if (migrationScriptFile.exists())
    migrationScriptFile.delete()

def repo = (config.repo ?: "$workDir/repo") + "/${proj.toLowerCase()}"

//******************************************************************************
//* Part of running this groovy script, it outputs members to files mapping.
//* This information is useful for generate an optimized script mapping file
//******************************************************************************
memberToFileFile = new File("$workDir/memberToFile.txt")
if (memberToFileFile.exists())
    memberToFileFile.delete()

def langextFile = new File(workDir, 'langext.txt')
fileExtMap = langextFile.inject([:]) { result, line ->
    def segments = line.split(":")
    if (segments.length == 3)
    {
        def lang = segments[0]
        def type = segments[1]
        def ext = segments[2].startsWith('.') ? segments[2].drop(1) : segments[2]
        (selectionCriteria == 'TYPE') ? result.put(type,ext) : result.put(lang,ext)
    }
    result
}

//******************************************************************************
//*  Generate script to initialize GIT repository
//******************************************************************************
migrationScriptFile << '#!/bin/sh' << '\n\n'
migrationScriptFile << "export DBB_HOME=$dbbHome" << '\n'
migrationScriptFile << 'currDir=`cd "$(dirname "$0")" && pwd`' << '\n'
migrationScriptFile << "repo=${repo}" << '\n\n'
migrationScriptFile << 'rm -rf $repo' << '\n'
migrationScriptFile << 'git init $repo' << '\n\n'

migrationScriptFile << "outputFile=$memberToFileFile" << '\n'
migrationScriptFile << 'rm -f $outputFile' << '\n\n'

def membersFile = new File("$workDir/members.txt")
//******************************************************************************
//* If the selectionCriteria is LANG, then we need additional tag file.
//******************************************************************************
if (selectionCriteria == 'LANG')
{
    migrationScriptFile << "tagFile=$membersFile" << '\n\n'
}

def currentVersion = ""
def newFilesAdded = false
def Set datasets = []
def Set langs = []

//******************************************************************************
//* Parse the members from the members.txt file and sort the members
//* based on each version
//******************************************************************************
def members = []
def curmems = []
membersFile.eachLine { members << it }
members = members.sort()
members = members.reverse()
curmems = members

def bndLangs = [] as Set
//******************************************************************************
//* Loop through each version member and record the member information
//* (data set, lang, version), when a new version is seen, then generate script
//* to migrate all members in previous version.
//******************************************************************************
members.each {

    if (it ==~ /.*\.V\d\..*/) {
        (version, dataset, member, lang) = parseLine(it)

        if (newFilesAdded && currentVersion != version)
        {
            if (!currentVersion.isEmpty())
            {
                // Add to GIT and commit the current batch
                outputMigrateAndCommitScript(currentVersion, datasets, langs)
                newFilesAdded = false
            }
            currentVersion = version
        }

        langs << lang
        datasets << dataset
        newFilesAdded = true
        currentVersion = version
    }
    else {
        (dsn,lang) = it.split(' ')
        bndLangs << lang
    }
}


//******************************************************************************
//* Loop through each current member and record the member information
//* (data set, lang, version), when a new version is seen, then generate script
//* to migrate all members in previous version.
//******************************************************************************
curmems.each {

    if (it ==~ /.*\.VCUR..*/) {
        (version, dataset, member, lang) = parseLine(it)

        if (newFilesAdded && currentVersion != version)
        {
            if (!currentVersion.isEmpty())
            {
                // Add to GIT and commit the current batch
                outputMigrateAndCommitScript(currentVersion, datasets, langs)
                newFilesAdded = false
            }
            currentVersion = version
        }

        langs << lang
        datasets << dataset
        newFilesAdded = true
    }
    else {
        (dsn,lang) = it.split(' ')
        bndLangs << lang
    }
}

// Add and commit the last batch
if (newFilesAdded)
{
    outputMigrateAndCommitScript(currentVersion, datasets, langs)
}


// Add .gitattributes and commit
migrationScriptFile << 'cd $repo' << '\n'
migrationScriptFile << 'git add .gitattributes' << '\n'
migrationScriptFile << "git commit -m 'GIT attributes'" << '\n'
migrationScriptFile << 'cd $currDir' << '\n'

Files.setPosixFilePermissions(Paths.get("$migrationScriptFile"), PosixFilePermissions.fromString("rwxrwxr-x"));

println "Successfully generate migration script $migrationScriptFile"

//******************************************************************************
// This function generates script to migrate and perform a GIT commit for
// each version of the migrated files.
//******************************************************************************
def outputMigrateAndCommitScript(def currentVersion, def datasets, def langs)
{
     migrationScriptFile << "echo 'Migrate files in version $currentVersion'" << '\n'
     if (selectionCriteria == 'LANG')
     {
         langs.each { lang ->
             def ext = fileExtMap["$lang"]
             if (ext)
                 migrationScriptFile << '$DBB_HOME/migration/bin/migrate.sh -r $repo ' << '-o $outputFile' << " -m MappingRule[hlq:${tempHlq}.${proj}.${group}.${currentVersion},toLower:true,extension:$ext,tagFile:" << '$tagFile]' <<" $lang" << '\n'
             else
                 migrationScriptFile << '$DBB_HOME/migration/bin/migrate.sh -r $repo ' << '-o $outputFile' << " -m MappingRule[hlq:${tempHlq}.${proj}.${group}.${currentVersion},toLower:true,tagFile:" << '$tagFile]' <<" $lang" << '\n'
         }
     }
     else  //selectionCriteria == 'TYPE'
     {
         if (datasets.size() > 0) {
         datasets.each { dataset ->
             (proj,group,currentVersion,type) = dataset.trim().split("\\.")
             def ext = fileExtMap["$type"]
             if (ext)
                 migrationScriptFile << '$DBB_HOME/migration/bin/migrate.sh -r $repo ' << '-o $outputFile' << " -m MappingRule[hlq:${tempHlq}.${proj}.${group}.${currentVersion},toLower:true,extension:$ext] $type" << '\n'
             else
                 migrationScriptFile << '$DBB_HOME/migration/bin/migrate.sh -r $repo ' << '-o $outputFile' << " -m MappingRule[hlq:${tempHlq}.${proj}.${group}.${currentVersion},toLower:true] $type" << '\n'
         }
         }
     }

     migrationScriptFile << "echo 'Add files to repository and commit'" << '\n'
     migrationScriptFile << 'cd $repo' << '\n'
     migrationScriptFile << 'git add *' << '\n'
     migrationScriptFile << "git commit -m '$currentVersion'" << '\n'
     migrationScriptFile << 'cd $currDir' << '\n\n'

     datasets.clear()
     langs.clear()
 }

//******************************************************************************
// This function parse each line in the version list to:
//   HLQ.Vx.DATASET(MEMBER)
//******************************************************************************
def parseLine(def line)
{
    (segment1, segment2) = line.split(" ")
    def lang = segment2.trim()
    def lessHlq = segment1.trim().minus(tempHlq + '.')
    def tokens = lessHlq.tokenize(".")
    def version = tokens.get(2)
    def lessVersion = lessHlq.minus(~/V\d+\./)
    (dataset,member) = lessHlq.split("[\\(\\)]")
    return [version, dataset, member, lang]
}
