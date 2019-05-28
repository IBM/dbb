import java.nio.file.*
import groovy.transform.*
import groovy.util.CliBuilder
import java.nio.file.attribute.*
import com.ibm.dbb.*

/*
 * This is the main class that generates a build.groovy and script.groovy
 * based on the BUILD.template and SCRIPT.template.  For the <build> and each
 * of the <script> nodes, it then goes through the BUILD.template and SCRIPT.template
 * respectively, line by line and search for any specified converterNames, ie:
 * ${DBBBuildTemplateConverter}.xxx, ${DBBScriptTemplateConverter}.yyy, and then
 * call those xxx() or yyy() methods to generate the Groovy code.  
 */

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
def headerMsg = 'Generate build scripts from DBB XML file'
cli = new CliBuilder(usage: 'GenerateBuildScripts', header: headerMsg, stopAtNonOption: false)
cli.h(longOpt:'help', 'Prints this message')
cli.c(longOpt:'config', args:1, argName:'config file', optionalArg:false, 'Configuration file: default is conf/jclmig.config')

def parameters = cli.parse(args)
if (!parameters || parameters.h)
{
	cli.usage()
	System.exit(2)
}

@SourceURI
URI sourceUri
scriptLocation = Paths.get(sourceUri).parent

//******************************************************************************
//* Parses the JCL migration config file
//******************************************************************************
def configFileName = (parameters.c && !parameters.c.isEmpty())? parameters.c: '../conf/jclmig.config'
def configFile = scriptLocation.resolve(configFileName).toFile()
if (!configFile.exists())
{
    println "File $configFile does not exist. Need to specify a valid JCL migration config file"
    System.exit(1)
}

//******************************************************************************
//* Parses the JCL migration config file
//******************************************************************************
def config = new Properties()
config.load(configFile.newDataInputStream())
def proj = config.proj
def workDir = new File(config.outputDir ?: System.getProperty('user.home'))
def outputDir = new File("$workDir/jclMigration/${proj.toLowerCase()}")
def dbbXmlFile = new File(outputDir, 'dbb.xml')
def saveJCLOutputs = (config.saveJCLOutputs && config.saveJCLOutputs.toBoolean())?true:false
def genExecVars = (config.genExecVars && config.genExecVars.toBoolean())?true:false

println "Process XML file: $dbbXmlFile"

def outputFiles = []

//********************************************************************************
//* Parse the input XML file
//********************************************************************************
def buildXml = new XmlParser().parse(dbbXmlFile.newReader())

//********************************************************************************
//* Create a shared Binding to pass to children scripts
//********************************************************************************
@Field def sharedData = new Binding()
sharedData.setVariable('buildXml', buildXml)
sharedData.setVariable('dbbXmlFile', dbbXmlFile)
sharedData.setVariable('outputDir', outputDir)
sharedData.setVariable('saveJCLOutputs', saveJCLOutputs)
sharedData.setVariable('genExecVars', genExecVars)

//********************************************************************************
//* Convert <properties>
//********************************************************************************
buildXml.propertyFiles.propertyFile.each { propertyFile ->
            
    if (propertyFile.@name)
    {
        def fileName = propertyFile.@name
        def fileDesc = propertyFile.@description
        def file = new File(outputDir, "${fileName}.properties")
        !file.exists()?:file.delete()

        if (fileDesc)
            file << "#$fileDesc" << '\n'

        propertyFile.property.each { property ->
            def name = property.@name
            def value = property.@value
            def pattern = property.@pattern
            def description = property.@description

            if (description)
                file << "# $description" << '\n'
            file << "$name = $value"
            if (pattern)
                file << " :: $pattern"
            file << '\n\n'
        }
        
        outputFiles << file
    }
}


//********************************************************************************
//* Generate build shell script to invoke the main build file
//********************************************************************************
def buildScriptName = convertToJavaIdentifier(buildXml.@name)
def buildShellScriptFile = new File(outputDir, "${buildScriptName}.sh")
!buildShellScriptFile.exists()?:buildShellScriptFile.delete()
buildShellScriptFile << "#!/bin/sh" << '\n\n'
buildShellScriptFile << "# Check that DBB_HOME is set" << '\n'
buildShellScriptFile << "if [[ -z \"\${DBB_HOME}\" ]]; then" << '\n'
buildShellScriptFile << "  echo \"Need to specified the required environment variable 'DBB_HOME'\"" << '\n'
buildShellScriptFile << "  exit 8" << '\n'
buildShellScriptFile << "fi" << '\n\n'
buildShellScriptFile << '# $DBB_HOME/bin/groovyz automatically sets the env variables and classpath required for DBB' << '\n'
buildXml.scripts.script.each { script ->
	def scriptName = convertToJavaIdentifier(script.@name)
	buildShellScriptFile << 'CMD=\"$DBB_HOME/bin/groovyz ' << "${scriptName}.groovy\"" << '\n\n'
	buildShellScriptFile << '$CMD' << '\n'
}
Files.setPosixFilePermissions(Paths.get("$buildShellScriptFile"), PosixFilePermissions.fromString("rwxrwxr-x"));

outputFiles << buildShellScriptFile

//********************************************************************************
//* Convert <scripts>
//********************************************************************************
def SCRIPT_CONVERTER_NAMES = ['DBBScriptTemplateConverter']
def scriptConverters = [:]

def scriptTemplateFile = scriptLocation.resolve('../templates/SCRIPT.template').toFile()

buildXml.scripts.script.each { script ->
	def scriptName = convertToJavaIdentifier(script.@name)
    def scriptFile = new File(outputDir, "${scriptName}.groovy")
    !scriptFile.exists()?:scriptFile.delete()
    
    sharedData.setVariable('scriptXml', script)    
            
    scriptTemplateFile.eachLine { line ->
        line = convertLine(line, SCRIPT_CONVERTER_NAMES, scriptConverters)
        scriptFile << line << '\n'        
    }
    
    scriptConverters.clear() 
    
    outputFiles << scriptFile           
}

println "There are ${outputFiles.size()} files generated in directory $outputDir:"
outputFiles.toSorted().collect { it.name }.each {
    println "   $it"
}

//********************************************************************************
//* Load a script and pass in the shared Binding data
//******************************************************************************** 
def loadConverter(String converterName)
{
    def shell = new GroovyShell(sharedData)
    shell.parse(scriptLocation.resolve("${converterName}.groovy").toFile())    
}

def convertLine(def line, def converterNames, def converters)
{
    //* Find a matched converter in the line from a list of supported converters
    def matchedConverterName = converterNames.find { name ->
        line.indexOf('${' + name + '.') > -1
    }
    
    //* If the line contains a matched converter then process
    //* the line by calling the converter's method
    if (matchedConverterName)
    {                
        int matchedIndex = line.indexOf('${' + matchedConverterName + '.')
        if (matchedIndex > -1)
        {
            //* We assume the format is something like ${Converter.method()}
            int endMatchedIndex = line.indexOf('}', matchedIndex)
            def temp = line.substring(matchedIndex, endMatchedIndex+1)
            def segments = temp.split('[\\$\\{\\.\\(\\)\\}]')
            def matchedSegments = segments.findAll {
                it.trim().length() > 0
            }

            if (matchedSegments.size() == 2)
            {
                def converterName = matchedSegments[0]
                def methodName = matchedSegments[1]
                
                def converter = converters.get(converterName)
                if (converter == null)
                {
                    converter = loadConverter(converterName)
                    if (converter.getMetaClass().respondsTo(converter, 'init'))
                        converter.init()
                    converters."$converterName" = converter
                }

                def replacement = converter."$methodName"()
                line = replacement ? line.replace('${' + converterName + '.' + methodName + "()}", replacement) : ""
            }
        }
    }
    
    line
}


/*
 * Utility method to convert an executor name into
 * a valid Java/Groovy method name.
 */
def convertToJavaIdentifier(text)
{
    def newText = ''
    text.getChars().eachWithIndex { ch, index ->
        boolean isValid = (index == 0 ? Character.isJavaIdentifierStart(ch) : Character.isJavaIdentifierPart(ch))
        newText += (isValid ? ch : '_')
    }
    newText
}