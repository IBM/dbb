import com.ibm.dbb.*
import com.ibm.dbb.build.*
import com.ibm.jzos.*
import java.nio.file.*
import groovy.transform.SourceURI
import groovy.cli.commons.*
import groovy.xml.*

/*******************************************************************************
 *
 * This script is used to extract information from SCLM to temporary locations
 * (data sets and HFS files).
 *
 * These are the information being extracted from SCLM:
 * 1. It first parses the information from the sclmmig.config.
 * 2. If user specifies a "step" to execute then skip to the next, otherwise
 *    it determines which "step" to execute based on the existence of the
 *    output files.
 * 3. Deletes any existing output files in case the previous run failed.
 * 4. Copy the Rexx execute associated with this "step" to the temporary
 *    data set along with the sclmmig.config.
 * 5. Invoke ISPFExec to run the Rexx execute from the data set.
 *
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

//******************************************************************************
//* Parses and validates the input arguments
//******************************************************************************
def headerMsg = 'Extract information of a SCLM project to the temporary data sets and files on HFS'
cli = new CliBuilder(usage: 'SclmExtract [options] sclmmig.config', header: headerMsg, stopAtNonOption: false)
cli.x(longOpt:'exec', args:1, argName:'rexxExec', 'The target executed REXX')
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

def config = new Properties()
config.load(configFile.newDataInputStream())

def proj = config.proj
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
def tempHlq = "${migHlq}.SCLMMIG"
def outputDir = new File(config.outputDir ?: System.getProperty('user.home'))
def versionLimit = config.versionLimit ?: -1

if (versionLimit == 0)
{
    cli.usage()
    System.exit(2)
}

//******************************************************************************
//* Retrieves the Rexx execute that user wants to run, if it is not specified
//* then tries to determine the next Rexx execute to run based on the
//* available output files.
//******************************************************************************

def targetDir = "$outputDir/sclmMigration/${proj.toLowerCase()}"
def rexxExecs = [EXTARCH:['langext.txt','projseq.txt','archtype.txt','keyref.xml','members.xml'], EXTMEMB:[], GENDEF:['systemDefinition.xml','fileMetaData.xml'], MEMCNT:['members.txt'],EXTSRC:[]]

def rexxExec
def rexxExecToRun = parameters.x
if (rexxExecToRun)
{
    rexxExec = rexxExecs.find { exec, outputs ->
        exec == rexxExecToRun
    }
}
else
{
    rexxExec = rexxExecs.find { exec, outputs ->
        def outputNotExist = outputs.findAll { output -> !new File("$targetDir/$output").exists() }
        if (outputNotExist.size())
        {
            //println "Execute $exec because expected output files '${outputNotExist.join(',')}' are not found"
            true
        }
        else false
    }
}

if (!rexxExec)
{
    println "Either user does not specify a Rexx step to execute or all SCLM information has been extracted"
    System.exit(0)
}

println "Executes ${rexxExec.key}"

//******************************************************************************
//* Delete all previous generated output files
//******************************************************************************
if (outputDir.exists())
{
    //println "Deletes any existing output files in directory $targetDir"
    rexxExec.value.each { output ->
        new File("$targetDir/$output").delete()
    }
}
else
{
    outputDir.mkdirs()
}

//******************************************************************************
//* Create a temporary data set and copy the Rexx execute and sclmmig.config
//* to this data set
//******************************************************************************
def rexxDataset = "${tempHlq}.REXX"
println "Creates data set '$rexxDataset'"
new CreatePDS().dataset(rexxDataset).options('CYL SPACE(1,5) LRECL(80) RECFM(F,B) BLKSIZE(32720) DSORG(PO) DSNTYPE(LIBRARY)').execute()

def rexxFileLocation = scriptLocation.resolve("../rexx").toFile()
def rexxExecName = rexxExec.key
def rexxFile = new File(rexxFileLocation, "${rexxExecName}.rexx")
println "Copies file $rexxFile to $rexxDataset($rexxExecName)"
new CopyToPDS().file(rexxFile).dataset(rexxDataset).member(rexxExecName).execute()

def sortDBFile = new File(rexxFileLocation, "SORTDB.rexx")
println "Copies $sortDBFile macro to $rexxDataset(SORTDB)"
new CopyToPDS().file(sortDBFile).dataset(rexxDataset).member('SORTDB').execute()

def sortVerFile = new File(rexxFileLocation, "SORTVER.rexx")
println "Copies $sortVerFile macro to $rexxDataset(SORTVER)"
new CopyToPDS().file(sortVerFile).dataset(rexxDataset).member('SORTVER').execute()

def VerFile = new File(rexxFileLocation, "VERRETR.rexx")
println "Copies $VerFile macro to $rexxDataset(VERRETR)"
new CopyToPDS().file(VerFile).dataset(rexxDataset).member('VERRETR').execute()

println "Copies $configFile to $rexxDataset(MIGCFG)"
new CopyToPDS().file(configFile).dataset(rexxDataset).member('MIGCFG').execute()

def skelDataset = "${tempHlq}.SKELS"
println "Creates data set '$skelDataset'"
new CreatePDS().dataset(skelDataset).options('CYL SPACE(1,5) LRECL(80) RECFM(F,B) BLKSIZE(32720) DSORG(PO) DSNTYPE(LIBRARY)').execute()

def jobcFile = new File(rexxFileLocation, "VERJOBC.skel")
println "Copies $jobcFile macro to $skelDataset(VERJOBC)"
new CopyToPDS().file(jobcFile).dataset(skelDataset).member('VERJOBC').execute()

def retrFile = new File(rexxFileLocation, "VERRETR.skel")
println "Copies $retrFile macro to $skelDataset(VERRETR)"
new CopyToPDS().file(retrFile).dataset(skelDataset).member('VERRETR').execute()

//******************************************************************************
//* Execute the REXX exec
//******************************************************************************
println "Executes $rexxDataset($rexxExecName)"
def logDir = new File("$targetDir/logs")
logDir.exists()?:logDir.mkdirs()
def logFile = new File(logDir, "${rexxExecName}.log")
def step = new ISPFExec().confDir(dbbConf).logFile(logFile).logEncoding('Cp1047').keepCommandScript(false)
step.command("EX '${rexxDataset}($rexxExecName)'")
step.addDDStatment("CMDSCP", "${tempHlq}.ISPFGWY.EXEC", "TRACKS SPACE(1,1) LRECL(270) RECFM(F,B) DSORG(PS)", false)
def rc = step.execute()

//******************************************************************************
//* Display the result
//******************************************************************************
if (rc)
{
    println "Failed to run $rexxDataset($rexxExecName), rc = $rc.  See $logFile for more details"
    System.exit(1)
}

def processFileInplace(file, Closure processText) {
    def text = file.text
    file.write(processText(text))
}

if (rexxExec.key == 'EXTSRC') {
    println "Additional Processing"
    def metadataFile = new File(targetDir, 'fileMetaData.xml')
    def parser = new XmlSlurper().parse(metadataFile)

    def langdefrules = parser."**".findAll { node ->
        node.name() == 'langdefrule' && node.@match.toString().contains('bnd')
    }

    def memberToLang = [:]
    langdefrules.each { langdefrule ->
        def member = langdefrule.@match.toString() - '.*/' - '\\.bnd$'
        def langdef = langdefrule.@languageDefinition.toString() - '${resource.def.prefix}' - '${resource.def.suffix}'
        memberToLang."$member" = langdef
    }

    def newContent = []

    def membersFile = new File(targetDir, 'members.txt')
    processFileInplace(membersFile) { text ->
        text.eachLine { line ->
            if (line.split(' ').length == 1) {
                def member = line.trim().drop(line.indexOf('(')+1) - ')'
                def langdef = memberToLang."$member"
                newContent << "$line $langdef"
            }
            else {
                newContent << line
            }
        }
        newContent.join('\n')
    }
}

println "Successfully executed $rexxDataset($rexxExecName)"
println "The following files were generated in directory $targetDir:"
rexxExec.value.each { output ->
    println "   $output"
}
//println "You can force to re-run this step ${rexxExec.key} by deleting any of the above output files"

System.exit(0)
