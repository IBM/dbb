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
def headerMsg = 'Convert JCL to DBB XML'
cli = new CliBuilder(usage: 'JCLtoDBBXml  dataset member', header: headerMsg, stopAtNonOption: false)
cli.h(longOpt:'help', 'Prints this message')
cli.c(longOpt:'config', args:1, argName:'config file', optionalArg:false, 'Configuration file: default is conf/jclmig.config')

def parameters = cli.parse(args)
if (!parameters || parameters.h || parameters.arguments().size() < 2)
{
	cli.usage()
	System.exit(2)
}

@SourceURI
		URI sourceUri
Path scriptLocation = Paths.get(sourceUri).parent

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

//******************************************************************************
//* Parses and validates the input arguments
//******************************************************************************
def dataset = parameters.arguments()[0]
def member = parameters.arguments()[1]

//******************************************************************************
//* Define workDir and outputDir for process output
//******************************************************************************
def proj = config.proj
def workDir = new File(config.outputRoot ?: System.getProperty('user.home'))
def outputDir = new File("$workDir/jclMigration/${proj.toLowerCase()}")
outputDir.mkdirs()
def stdout = new File(outputDir, "stdout.log")
def stderr = new File(outputDir, "stderr.log")
def tempHlq = (config.tempHlq?:"whoami".execute().text).trim() + ".JCLMIG"
def restrictedPgms = ['IKJEFT01','IKJEFT1A','IKJEFT1B','IRXJCL','IOEAGFMT']
restrictedPgms.addAll((config.restrictedPgms?config.restrictedPgms.split(","):[]))

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
def cmd = "$dbbHome/bin/dmh4000 -x $dataset $member"
def proc = cmd.execute(null, outputDir)
proc.waitForProcessOutput(sout, serr)
stdout << sout
stderr << serr
proc.waitForOrKill(1000)

//******************************************************************************
//* Parses the JCL parser output file
//******************************************************************************
def parserOutputFile = new File(outputDir, 'DD:ATTRBOUT')
def dbbXmlFile = new File(outputDir, 'dbb.xml')
dbbXmlFile.exists() ? dbbXmlFile.delete() : false

def content = parserOutputFile.text
project = new XmlSlurper().parseText(content)

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
			options << "${concat.dispnor}".toLowerCase()
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
						options << "directory(${allocations[2]})"
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