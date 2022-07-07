import java.nio.file.*
import groovy.transform.*
import groovy.cli.commons.*
import groovy.xml.*
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
cli = new CliBuilder(usage: 'GenerateBuildScripts sclmmig.config', header: headerMsg, stopAtNonOption: false)
cli.h(longOpt:'help', 'Prints this message')

def parameters = cli.parse(args)
if (parameters.arguments().size() < 1)
{
    cli.usage()
    System.exit(2)
}

@SourceURI
URI sourceUri
scriptLocation = Paths.get(sourceUri).parent

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
def workDir = new File(config.outputDir ?: System.getProperty('user.home'))
workDir = new File("$workDir/sclmMigration/${proj.toLowerCase()}")
def dbbXmlFile = new File(workDir, 'dbb.xml')
def repoDir = config.repo ?: "$workDir/repo"
def outputDir = new File("$repoDir/${proj.toLowerCase()}/build")

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
//* Convert <build>
//********************************************************************************
 
def BUILD_CONVERTER_NAMES = ['DBBBuildTemplateConverter']
def buildConverters = [:]

def buildXmlName = convertToJavaIdentifier(buildXml.@name)

//* Delete the existing output file if one exists
def buildFile = new File(outputDir, "${buildXmlName}.groovy")
!buildFile.exists()?:buildFile.delete()

//* Read through the Build template file and generate a build groovy script
//* based on the input XML contents
def buildTemplateFile = scriptLocation.resolve('../templates/BUILD.template').toFile()
buildTemplateFile.eachLine { line ->
    
    line = convertLine(line, BUILD_CONVERTER_NAMES, buildConverters)    
    buildFile << line << '\n'    
}

outputFiles << buildFile

/*
 * Generate build.sh to invoke the main build file
 */
def buildShellScriptFile = new File(outputDir, 'build.sh')
!buildShellScriptFile.exists()?:buildShellScriptFile.delete()
buildShellScriptFile << "#!/bin/sh" << '\n\n'
buildShellScriptFile << "# Set the DBB bin directory" << '\n'
buildShellScriptFile << "DBB_HOME=$dbbHome" << '\n\n'
buildShellScriptFile << '# $DBB_HOME/bin/groovyz automatically sets the env variables and classpath required for DBB' << '\n'
buildShellScriptFile << 'CMD=\"$DBB_HOME/bin/groovyz ' << "${buildXmlName}.groovy files.txt\"" << '\n\n'
buildShellScriptFile << '$CMD' << '\n'
Files.setPosixFilePermissions(Paths.get("$buildShellScriptFile"), PosixFilePermissions.fromString("rwxrwxr-x"));

outputFiles << buildShellScriptFile


//********************************************************************************
//* Convert <scripts>
//********************************************************************************
def SCRIPT_CONVERTER_NAMES = ['DBBScriptTemplateConverter']
def scriptConverters = [:]

def scriptTemplateFile = scriptLocation.resolve('../templates/SCRIPT.template').toFile()

buildXml.scripts.script.each { script ->
    def scriptFile = new File(outputDir, "${script.@name}.groovy")
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