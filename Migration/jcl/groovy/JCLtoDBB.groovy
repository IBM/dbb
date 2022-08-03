/*
 * Generate the DBB.xml file from the systemDefinition.xml file.
 * 
 * Limitation: we ignore resourcePrefix and resourceSuffix.  RTC uses
 * this prefix and suffix to append to the language definition name.
 * We could do the same with the name of the script file, but that also
 * requires us to generate the scriptMappings.txt that has the same language
 * definition names matching with what in DBB.xml file.   
 */
@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import groovy.util.*
import groovy.transform.*
import groovy.time.*
import groovy.xml.*
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
def headerMsg = 'Convert JCL to DBB'
 
def parameters = parseArgs(args)

@SourceURI
		URI sourceUri
Path scriptLocation = Paths.get(sourceUri).parent

//******************************************************************************
//* Parses the JCL migration config file
//******************************************************************************
//if (!parameters.c.startsWith('/'))
//	parameters.c = '../'parameters.c
	
def configFileName = parameters.c

if (!configFileName) {
	configFileName = '../conf/jclmig.config'	
}

def configFile     = scriptLocation.resolve(configFileName).toFile()
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

//******************************************************************************
//* Parses and validates the input arguments
//******************************************************************************
def dataset = parameters.d
def member  = parameters.m

//******************************************************************************
//* Define outputDir for process output
//******************************************************************************
def proj      = parameters.p
def outputDir = parameters.o

if (!outputDir) {
	outputDir = 'jclMigration'
}
if (!outputDir.startsWith('/'))
{
	homeDir = new File(System.getProperty('user.home'))
	outputDir = "$homeDir/$outputDir"
}
outputDir          = new File("$outputDir/${proj.toLowerCase()}")
outputDir.mkdirs()
def stdout         = new File(outputDir, "stdout.log")
def stderr         = new File(outputDir, "stderr.log")
def tempHlq        = (config.tempHlq?:"whoami".execute().text).trim() + ".JCLMIG"
def restrictedPgms = ['IKJEFT01','IKJEFT1A','IKJEFT1B','IRXJCL','IOEAGFMT']
restrictedPgms.addAll((config.restrictedPgms?config.restrictedPgms.split(","):[]))
def procLibs       = []
procLibs.addAll((config.procLibs?config.procLibs.split(","):[]))

def ddFile = outputDir.path + '/DD:JCLLIB'

boolean fileSuccessfullyDeleted =  new File(ddFile).delete()
println 'DD file deleted = ' + fileSuccessfullyDeleted

File ddFileOut = new File(ddFile) //.withWriter('IBM-1047')
def fileText = ''

procLibs.each {
	it       = it.trim()
	fileText = fileText + it.padRight(79) + '\n'
}


ddFileOut.text = fileText

// define the BPXWDYN options for allocated temporary datasets
tempCreateOptions = (config.tempDataSetOptions?:"cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new").trim()

println "Storing output to $outputDir"

//******************************************************************************
//* Parse JCL
//******************************************************************************
def sout = new StringBuffer(), serr = new StringBuffer()

def touch ='touch DD:JCLLIB'.execute(null, outputDir)
touch.waitForProcessOutput(sout, serr)
touch.waitForOrKill(1000)

println "Parsing $dataset($member)"
def cmd = "$dbbHome/lib/dmh4000 -x $dataset $member"
def proc = cmd.execute(null, outputDir)
proc.waitForProcessOutput(sout, serr)
stdout << sout
stderr << serr
proc.waitForOrKill(1000)

//******************************************************************************
//* Parses the JCL parser output file
//******************************************************************************
def parserOutputFile = new File(outputDir, 'DD:ATTRBOUT')
def dbbXmlFile       = new File(outputDir, 'dbb.xml')
dbbXmlFile.exists() ? dbbXmlFile.delete() : false

def content = parserOutputFile.text
project     = new XmlSlurper().parseText(content)

def rc = project.file.jcl.returnCode.text().toInteger()
if ( rc != 0 )
{
	def m = "${project.file.jcl}" =~ /.*"(.*)".*/;
	m.each{ match ->
		println match[1]
	}
	if ( rc >= 8 )
		System.exit(rc)
}


/*
 * Find all steps
 */
def steps = project."**".findAll { node ->
	node.name() == "step"
}

dsTypeOptions = [
	'dsorg(PO) dsntype(LIBRARY)',
	'dsorg(PS)',
	'',
	'dsorg(PO) dsntype(PDS)'
]
validRecfm = [
	'A',
	'B',
	'D',
	'F',
	'M',
	'S',
	'T',
	'U',
	'V'
]
spaceUnitConversion = ['trk':'tracks', 'cyl':'cyl', 'abstr':'']  //TODO : missing ABSTR
datasetNameConversion = ['STEPLIB':'TASKLIB']

println "Restricted programs: ${restrictedPgms}"

//******************************************************************************
//* Write dbb.xml
//******************************************************************************
def xmlBuilder = new StreamingMarkupBuilder()
xmlBuilder.encoding = 'UTF-8'

def xml = {
	mkp.xmlDeclaration()
	mkp.yieldUnescaped(" <!-- This file is generated from $parserOutputFile -->")

	build(name: "${project.file.name}".toLowerCase(), source : "${project.name}(${project.file.name})") {
		scripts() {
			script (name : "${project.file.name}".toLowerCase()) {
				steps.each { step ->
					println "Processing step ${step.name}"
					def isRestricted = ( restrictedPgms.find{e-> e == "${step.exec.name}"} != null )
					if ( isRestricted )
					{
						println "WARNING: Program ${step.exec.name} may require special authority. The generated exec command may need to be modified. Search on TODO: in the generated groovy script."
						def job = project.file.jcl.job
						// Define the JCL used in the JCLExec
						def jcl = []
						jcl << breakup("//${job.@name.text().padRight(8)} JOB ${job.jobcard.@data}")
						jcl << breakup("//${step.name.text().padRight(8)} EXEC PGM=${step.exec.name}${(step.parm.text().isEmpty())?"":",PARM=${step.parm}"}")
						step.dd.each { ddx ->
							def firstAllocation = ddx.concat.find{it.@sequence == "1"}
							jcl << "//${ddx.name.text().padRight(8)} DD ${firstAllocation.parm}"
							def ddm = convertAllocationToDD(firstAllocation)
							if (ddm.'instreamData')
							{
								def dlm = (ddm.'dlm')?ddm.'dlm':"/*"
								jcl << "${ddm.'instreamData'}$dlm"
							}
							ddx.concat.each { concat ->
								if (concat.@sequence != "1")
								{
									jcl << "//${"".padRight(8)} DD ${concat.parm}"
									ddm = convertAllocationToDD(firstAllocation)
									if (ddm.'instreamData')
									{
										def dlm = (ddm.'dlm')?ddm.'dlm':"/*"
										jcl << "${ddm.'instreamData'}$dlm"
									}
								}
							}
						}
						def name = "${step.name}_${(step.proc.text().isEmpty())?step.exec.name:step.proc}"
						execute(type: 'jcl', name : name, maxRC: 8,	text: jcl.join('\n'), confDir: dbbConf )
					}
					else
					{
						execute(type: 'mvs', name : "${step.name}_${(step.proc.text().isEmpty())?step.exec.name:step.proc}", maxRC: 8,
								parm: "${step.parm.text().replaceAll(/^'/,"").replaceAll(/'$/,"")}", pgm: "${step.exec.name}" )
						{
							step.dd.each { ddx ->
								def firstAllocation = ddx.concat.find{it.@sequence == "1"}
								dd(([name: datasetNameConversion["${ddx.name}"]?:"${ddx.name}"] + convertAllocationToDD(firstAllocation)))
								{
									ddx.concat.each { concat ->
										if (concat.@sequence != "1")
										{
											dd(convertAllocationToDD(concat))
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
}

XmlUtil.serialize( xmlBuilder.bind(xml), dbbXmlFile.newWriter())

println "Successfully generated $dbbXmlFile"

 //******************************************************************************
 //* Parses the JCL migration config file
 //******************************************************************************
 def saveJCLOutputs = (parameters.s && parameters.s.toBoolean())?true:false
 def genExecVars    = (parameters.g && parameters.g.toBoolean())?true:false
 
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
		 line = convertLine(line, SCRIPT_CONVERTER_NAMES, scriptConverters, scriptLocation)
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
 def loadConverter(String converterName, Path scriptLocation)
 {
	 def shell = new GroovyShell(sharedData)
	 shell.parse(scriptLocation.resolve("${converterName}.groovy").toFile())
 }
 
 def convertLine(def line, def converterNames, def converters, scriptLocation)
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
					 converter = loadConverter(converterName, scriptLocation)
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

return

def breakup(line)
{
	def lines = []
	if (line.length() > 71)
	{
		ndx = line.substring(0,71).lastIndexOf(',')
		lines << line.substring(0,ndx+1)
		lines << breakup("//         ${line.substring(ndx+1)}")
	}
	else
		lines << line
	lines.join('\n')
}

def convertAllocationToDD(def concat)
{
	dd = [:]
	def options = []
	isTemp = false
	if (!concat.dsn.text().isEmpty())
	{
		def dsn = "${concat.dsn}"
		if ( dsn.startsWith("&") && !dsn.startsWith("&&") )
		{
			println "WARNING: Temporary dataset, $dsn, is not supported by DBB. The name has been migrated to &$dsn."
			dsn = "&$dsn"
		}
		dd.'dsn' = dsn
	}
	if (!concat.stat.text().isEmpty())
	{
		options << "${concat.stat}".toLowerCase()
	}
	if (!concat.dispnor.text().isEmpty())
	{
		if (concat.dispnor.text() == "PASS")
			dd.'pass' = true
		else
		{
			def dispnorValue = "${concat.dispnor}".toLowerCase()
            //* Map the disposition catlg/uncatlg to catalog/uncatalog to align to BPXWDYN utility options
			if (dispnorValue == "catlg") {
				options << "catalog"
			}
			else if (dispnorValue == "uncatlg") {
				options << "uncatalog"
			} else {
				options << dispnorValue
			}			
		}
	}
	if (!concat.parm.text().isEmpty())
	{
		def parms = concat.parm.text()
		def parmlist = splitit(parms, ",")
		parmlist.each { parm ->
			if ( parm.startsWith("DSN") ) {} // ignore, already handled above
			else if ( parm.startsWith("DISP") ) {} // ignore, already handled with stat and dispnor above
			else if ( parm.startsWith("DLM") ) 
			{
				def value = parm.substring( "DLM=".length() )
				dd.'dlm' = value
			}
			else if (  parm == "*" || parm == "DATA" ) // in stream data
			{
				data = ""
				concat.data.each { line ->
					data += (line.text().length()<=72)?line.text():line.text().substring(0,72)
					data += "\n"
				}
				dd.'instreamData'= data
			}
			else if (parm.startsWith("DDNAME="))
			{
				def m = parms =~ /(.*)=(.*)/;
				def ddName = m[0][2]
				dd.'ddref' = ddName
			}
			else if ( parm.startsWith("DCB=") )
			{
				def value = parm.substring( "DCB=".length() )
				value = value.trim().replaceFirst("\\(", "").replaceAll("\\)\$", "")
				def dcblist = splitit(value, ",")
				dcblist.each { dcbparm ->
					options << processAllocOption(dcbparm)
				}
			}
			else if ( parm.startsWith("VOL") )
			{
				def value = parm.substring( parm.indexOf("=")+1 )
				options << processVolumeOption( value )
			}
			else if ( parm.startsWith("UNIT") )
			{
				def value = parm.substring( parm.indexOf("=")+1 )
				options << processUnitOption( value )
			}
			else if ( parm.startsWith("LABEL") )
			{
				def value = parm.substring( parm.indexOf("=")+1 )
				options << processLabelOption( value )
			}
			else if ( parm == "DUMMY" || parm.startsWith("SYSOUT=") )
			{
				isTemp = true
				dd.'output' = true
			}
			else
			{
				options << processAllocOption(parm)
			}
		}
	}
	if (!options.isEmpty())
		dd.'options' = options.join(' ')
	else if (isTemp)
		dd.'options' = tempCreateOptions
	dd
}

def processAllocOption( parm )
{
	def options = []
	def m = parm.toLowerCase() =~ /([a-z]*)=(.*)/;
	def key = m[0][1]
	def value = m[0][2]
	if (key == "space")
	{
		value = value.trim().replaceFirst("\\(", "").replaceAll("\\)\$", "")
		def spaceParms = splitit(value, ",")
		spaceParms.eachWithIndex { spaceParm, index ->
			switch (index)
			{
				case 0:
					def blk = spaceParm.trim()
					if (blk.length() > 0)
						options << ((blk.isInteger())? "block($blk)":"${spaceUnitConversion[blk]}")
					break
				case 1:
					def pri, sec, dir
					spaceParm = spaceParm.trim().replaceFirst("\\(", "").replaceAll("\\)\$", "")
					def allocations = splitit(spaceParm, ",")
					if (allocations.size() > 0 && allocations[0].trim().length() > 0)
					{
						def space = "$key(${allocations[0]}"
						if (allocations.size() > 1)
						{
							space += ",${allocations[1]}"
						}
						space += ")"
						options << space
					}
					if (allocations.size() > 2 && allocations[2].length() > 0)
					{
						options << "dir(${allocations[2]})"
					}
					break
				case 2:
					def rlse = spaceParm.trim()
				// TODO: What to do with this?
					break
				case 3:
					def contig = spaceParm.trim()
				// TODO: What to do with this?
					break
				case 4:
					def round = spaceParm.trim()
				// TODO: What to do with this?
					break
			}
		}
	}
	else if (key == "recfm")
	{
		def recfm="$value".toCharArray()
		value = recfm.join(',')
		options << "$key($value)"
	}
	else
		options << "$key($value)"
	options.join(' ')
}

def splitit( str, delimiter )
{
	def output = []
	def parens = 0;
	def value = ""
	str.each { ch ->
		if ( ch == delimiter && parens == 0)
		{
			output << value
			value = ""
		}
		else
		{
			value += ch
			if ( ch == '(' )
				parens++
			if ( ch == ')' )
				parens--
		}
	}
	if (value != "")
	{
		output << value
	}
	output
}

def processVolumeOption( value )
{
	def options = []
	value = value.trim().replaceFirst("\\(", "").replaceAll("\\)\$", "")
	def vollist = splitit(value, ",")
	vollist.each { volparm ->
		def m = volparm =~ /([a-z,A-Z]*)=?(.*)/;
		def volkey = m[0][1]
		if (volkey == "SER")
		{
			def servalue = m[0][2]
			servalue = servalue.trim().replaceFirst("\\(", "").replaceAll("\\)\$", "")
			def serlist = splitit(servalue, ",")
			def vol = []
			serlist.each{ ser ->
				vol << ser
			}
			options << "vol(${vol.join(',')})"
		}
	}
	options.join(' ')
}

def processLabelOption( value )
{
	def options = []
	value = value.trim().replaceFirst("\\(", "").replaceAll("\\)\$", "")
	def list = splitit(value, ",")
	list.eachWithIndex { parm, ndx ->
		def m = parm =~ /([a-z,A-Z,0-9]*)=?(.*)/;
		def key = m[0][1]
		if (ndx == 1 && !key.trim().isEmpty())
		{
			options << "label($key)"
		}
		else if (key == "RETPD")
		{
			def retpd = m[0][2]
			options << "retpd($retpd)"
		}
	}
	options.join(' ')
}

def processUnitOption( value )
{
	def options = []
	value = value.trim().toLowerCase().replaceFirst("\\(", "").replaceAll("\\)\$", "")
	def list = splitit(value, ",")
	list.eachWithIndex { parm, ndx ->
		def m = parm =~ /([a-z,A-Z,0-9]*)=?(.*)/;
		def key = m[0][1]
		if (ndx == 0 && !key.trim().isEmpty())
		{
			options << "unit($key)"
		}
	}
	options.join(' ')
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


//
//   Parse the command line arguments
//
def parseArgs(String[] args) {
	String usage = 'JCLtoDBB.groovy [options]'
	
	def cli = new CliBuilder(usage:usage)
		  cli.c(longOpt: 'configFile',    args:1, argName: 'configFile',                    optionalArg:true,  'Path to the JCL migration configuration file.  If specified, path is considered absolute if it begins with a slash else it is relative path from the migration tool bin directory.  Default is ../conf/jclmig.config.')
		  cli.d(longOpt: 'dataset',       args:1, argName: 'MVS dataset',                   optionalArg:false, 'Dataset containing JCL to be migrated (Required)')
		  cli.g(longOpt: 'genExecVars',   args:1, argName: 'Generate executable variables', optionalArg:true,  'Specify true to generate executable variables')
		  cli.h(longOpt: 'help',                                                                               'Show usage information')
		  cli.m(longOpt: 'member',        args:1, argName: 'JCL member',                    optionalArg:false, 'JCL member being migrated (Required)')
		  cli.o(longOpt: 'outputDir',     args:1, argName: 'output directory',              optionalArg:true,  'Directory in the HFS where all files will be written. If specified, path is considered absolute if it begins with a slash else it is relative path from the users home directory.  Default is jclMigration.')
		  cli.p(longOpt: 'project',       args:1, argName: 'JCL project',                   optionalArg:false, 'JCL project to be migrated (Required)')
		  cli.s(longOpt: 'saveOutputs',   args:1, argName: 'save JCLExec outputs',          optionalArg:true,  'Specify true to generated code to save outputs from a JCLExec')

    cli.width = 150  
	
	def opts = cli.parse(args)
	if (!opts) {
		System.exit(1)
	}
	
	// if help option used, print usage and exit
	if (opts.help) {
		cli.usage()
		System.exit(0)
	}
	if (!opts.p || !opts.d || !opts.m)
	{
		cli.usage()
		System.exit(0)
	}
	return opts
}
