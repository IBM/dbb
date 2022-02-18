/*
 * Generate the DBB.xml file from the systemDefinition.xml file.
 *
 * Limitation: we ignore resourcePrefix and resourceSuffix.  RTC uses
 * this prefix and suffix to append to the language definition name.
 * We could do the same with the name of the script file, but that also
 * requires us to generate the scriptMappings.txt that has the same language
 * definition names matching with what in DBB.xml file.
 */
import groovy.xml.*
import groovy.transform.*
import groovy.cli.commons.*
import java.nio.file.*
import java.nio.file.attribute.*
import com.ibm.dbb.*

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
def headerMsg = 'Convert System Definition XML to DBB XML'
cli = new CliBuilder(usage: 'GenerateDBBXml sclmmig.config', header: headerMsg, stopAtNonOption: false)
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
def proj = config.proj
def outputDir = new File(config.outputDir ?: System.getProperty('user.home'))
outputDir = new File("$outputDir/sclmMigration/${proj.toLowerCase()}")
def systemDefinitionXmlFile = new File(outputDir, 'systemDefinition.xml')

println "Process system definitions in $systemDefinitionXmlFile"

def dbbXmlFile = new File(outputDir, 'dbb.xml')

dbbXmlFile.exists() ? dbbXmlFile.delete() : false

def content = systemDefinitionXmlFile.text
/* def project = new XmlSlurper().parseText(content) */

/*
 * Retrieve the resource prefix and suffix for substitution
 */

def resourcePrefix = ''
def resourceSuffix = ''
/*
def resourcePrefix = project.property.find { property ->
    property.@name == 'resource.def.prefix'
}.@value.toString()
def resourceSuffix = project.property.find { property ->
    property.@name == 'resource.def.suffix'
}.@value.toString()
*/

//println "resourcePrefix=$resourcePrefix"
//println "resourceSuffix=$resourceSuffix"

def resourcePrefixRegex = ~/\$\{resource\.def\.prefix\}/
def resourceSuffixRegex = ~/\$\{resource\.def\.suffix\}/

content = content.replaceAll(resourcePrefixRegex, resourcePrefix)
content = content.replaceAll(resourceSuffixRegex, resourceSuffix)
content = content.replaceAll(~/\@\{source\.member\.name\}/, '\\\${MEMBER}')

project = new XmlSlurper().parseText(content)

/*
 * Find all dsdefs
 */


dsdefs_0 = project."**".findAll { node ->
    node.name() == "dsdef" && node.@dsDefUsageType == 0
}

dsdefs_1 = project."**".findAll { node ->
    node.name() == "dsdef" && node.@dsDefUsageType == 1
}
dsdefs_2 = project."**".findAll { node ->
    node.name() == "dsdef" && node.@dsDefUsageType == 2
}
dsdefs_3 = project."**".findAll { node ->
    node.name() == "dsdef" && node.@dsDefUsageType == 3
}
translators = project."**".findAll { node ->
    node.name() == "translator"
}
langdefs = project."**".findAll { node ->
    node.name() == "langdef"
}

//println "Language Definitions size: ${langdefs.size}"

dsTypeOptions = ['dsorg(PO) dsntype(LIBRARY)', 'dsorg(PS)', '', 'dsorg(PO) dsntype(PDS)']
validRecfm = ['A', 'B', 'D', 'F', 'M', 'S', 'T', 'U', 'V']
spaceUnitConversion = ['TRKS':'TRACKS', 'CYLS':'CYL', 'BLKS':'']  //TODO : missing RECLGTH

/*
 * Write build.xml
 */

def xmlBuilder = new StreamingMarkupBuilder()
xmlBuilder.encoding = 'UTF-8'

def xml = {
    mkp.xmlDeclaration()
    mkp.yieldUnescaped(" <!-- This file is generated from $systemDefinitionXmlFile -->")

    build(name : "${project.@name}") {

        //Generate default DBB properties files
        propertyFiles() {
            propertyFile(file : 'build.properties')
            propertyFile(file : 'files.properties')
            propertyFile(file : 'scriptMappings.properties')
            propertyFile(file : 'datasetMappings.properties')
        }

        //Generate <datasets>
        datasets() {
            (dsdefs_0 + dsdefs_1).each { dsdef ->
                def dsName = (dsdef.@prefixDSN.toBoolean() ? '${HLQ}.' : '') + dsdef.@dsName

                dataset(dsn : getFullyQualifiedDsn(dsdef), options : convertToBpxwdynOptions(dsdef))
            }
        }

        //Generate scripts
        def callMethodTypes = ['mvs', 'ispf', 'tso']
        scripts() {
            langdefs.each { langdef ->
                script (name : langdef.@name) {
                    langdef.@translators.toString().split(',').each { translatorName ->
                        def translator = translators.find { it.@name == translatorName }
                        if (translator) {
                            def dsdef = dsdefs_3.find {it.@name == translator.@dataSetDefinition}
                            def dsName = dsdef?.@dsName
                            def needTaskLib = (dsName && !dsName.text().isEmpty())
                            execute(type: callMethodTypes[translator.@callMethod.toInteger()], name : translator.@name, file : '${FILE}', maxRC: translator.@maxRC,
                            parm: translator.@defaultOptions, pgm: dsdefs_3.find {it.@name == translator.@dataSetDefinition}?.@dsMember, ddnames: translator.@ddnamelist) {
                                translator.allocation.each { dd(convertAllocationToDD(it))}
                                translator.concatenation.each { concatenation ->
                                    if (concatenation.allocation.size() > 0) {
                                        def firstAllocation = concatenation.allocation[0]
                                        dd(([name: concatenation.@name] + convertAllocationToDD(firstAllocation))) {
                                            concatenation.allocation.eachWithIndex { allocation, index ->
                                                if (index > 0)
                                                    dd(convertAllocationToDD(allocation))
                                            }
                                        }
                                    }
                                }
                                def needAddImplicitTaskLib = needTaskLib && translator.concatenation.find { it.@name.toString().toUpperCase() == 'TASKLIB' }.@name.text().isEmpty()
                                if (needAddImplicitTaskLib)
                                    dd(name : 'TASKLIB', dsn : dsName, options: 'shr')
                                translator.variable.each {
                                    def propValue = it.@value.toString()
                                    propValue = (propValue == '*' ? '${MEMBER}' : propValue)
                                    property(name : "${it.@name}", value : "${propValue}")
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}


XmlUtil.serialize(xmlBuilder.bind(xml), dbbXmlFile.newWriter())

println "Successfully generated $dbbXmlFile"




def convertToBpxwdynOptions(dsdef)
{
    //Create bpxwdyn options
    def options = []
    options << "${spaceUnitConversion[dsdef.@spaceUnits.toString().toUpperCase()]}"
    options << "space(${dsdef.@primaryQuantity.text().isEmpty() ? 0 : dsdef.@primaryQuantity.toInteger()},${dsdef.@secondaryQuantity.text().isEmpty() ? 0 : dsdef.@secondaryQuantity.toInteger()})"
    if (!dsdef.@directoryBlocks.text().isEmpty())
        options << "dir(${dsdef.@directoryBlocks.toInteger()})"
    if (!dsdef.@dsType.text().isEmpty() && dsdef.@dsType.toInteger() < 4)
        options << dsTypeOptions[dsdef.@dsType.toInteger()]
    options <<  "recfm(${dsdef.@recordFormat.text().isEmpty() ? 'F,B' : dsdef.@recordFormat.toString().toUpperCase().findAll { ch -> ch in validRecfm }.join(',')})"
    options << "lrecl(${dsdef.@recordLength.text().isEmpty() ? 80 : dsdef.@recordLength.toInteger()})"
    if (!dsdef.@storageClass.text().isEmpty())
        options << "STORCLAS(${dsdef.@storageClass.toString()})"
    if (!dsdef.@managementClass.text().isEmpty())
        options << "MGMTCLAS(${dsdef.@managementClass.toString()})"
    if (!dsdef.@dataClass.text().isEmpty())
        options << "DATACLAS(${dsdef.@dataClass.toString()})"
    if (!dsdef.@volumeSerial.text().isEmpty())
        options << "vol(${dsdef.@volumeSerial.toString()})"
    if (!dsdef.@genericUnit.text().isEmpty())
        options << "unit(${dsdef.@genericUnit.toString()})"
    options.join(' ').trim()
}


def convertAllocationToDD(def allocation)
{
    dd = [:]
    if (!allocation.@name.text().isEmpty())
        dd.'name' = allocation.@name
    if (allocation.@input.toBoolean()) {
        dd.'dsn' = '${HLQ}.${DSMAPPING}(${MEMBER})'
        dd.'options' = 'shr'
        dd.'report' = true
    }
    else {
        if (!allocation.@dataSetDefinition.text().isEmpty()) {
            def dsdef = (dsdefs_0 + dsdefs_1 + dsdefs_2 + dsdefs_3).find { it.@name == allocation.@dataSetDefinition}
            if (!dsdef)
                println "Cannot find ${allocation.@dataSetDefinition.text()}"

            def appendedMember = ''
            if (allocation.@member.toBoolean())
                appendedMember = '(${' + (allocation.@outputNameKind.toString() == 'USE_VARIABLE' ? allocation.@outputName.toString() : 'MEMBER') + '})'
            dd.'dsn' = getFullyQualifiedDsn(dsdef) + appendedMember
            dd.'options' = dsdef.@dsDefUsageType.toInteger() == 2 ? convertToBpxwdynOptions(dsdef) : 'shr'
        }
        if (!allocation.@output.text().isEmpty()) {
            dd.'output' = allocation.@output.toBoolean()
        }
        if (!allocation.@condition.text().isEmpty()) {
            dd.'condition' = convertAntEqualsCondition(allocation.@condition.toString())
        }
    }
    dd
}

def getFullyQualifiedDsn(dsdef)
{
    def usageType = dsdef.@dsDefUsageType.toInteger()
    def addHlq = (usageType != 2) && (usageType == 0 || (usageType == 1 && (dsdef.@prefixDSN.isEmpty() || dsdef.@prefixDSN.toBoolean())) || (usageType == 3 && dsdef.@prefixDSN.toBoolean()))
    def fullyQualifiedDsn = addHlq ? '${HLQ}.' + dsdef.@dsName.toString() : dsdef.@dsName.toString()
}

/*
 * Support for conditional DD.
 * Current Limitation : only support for <equal> condition.
 */
def convertAntEqualsCondition(def conditionText)
{
    def condition = new XmlSlurper().parseText(conditionText)
    if (condition.name().toString() == 'equals') {
        def arg1 = condition.@arg1.toString()
        def arg2 = condition.@arg2.toString()

        def output = []
        output << (arg1.startsWith('@{var.') ? arg1.drop(arg1.lastIndexOf('.')+1).minus('}') : "'$arg1'")
        output << '.equals'
        output << '('
        output << (arg2.startsWith('@{var.') ? arg2.drop(arg2.lastIndexOf('.')+1).minus('}') : "'$arg2'")
        output << ')'
        output.join('')
    }
}
