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
import groovy.yaml.YamlBuilder
import java.nio.charset.StandardCharsets
import groovy.yaml.YamlRuntimeException
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.ObjectMapper
import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator
import com.ibm.jzos.FileAttribute

class DSN {
	String DSN
	String options
	Boolean output
	Boolean pass
	String instreamData
	//String DLM // https://www.ibm.com/docs/en/zos/2.4.0?topic=statement-data-parameter a parameter specifying a delimiter that indicates when to stop reading instream data

	public String toString() {
		return String.format("DD '%s': options='%s', output='%s', pass='%b', instreamData='%s'", DSN, options, output, pass, instreamData)
	}

	public Map<String, Object> toYaml(Configuration config, String name) {
		Map<String, Object> ddMap = new LinkedHashMap<>()
		if (name != null) {
			if ("STEPLIB".equals(name.toUpperCase())) {
				name = "TASKLIB"
			}
			ddMap.put("name", name)
		}

		if ("SYSPRINT".equals(name)) {
			ddMap.put("log", "\${LOGS}/\${STEP}-\${FILE_NAME}.log")
			ddMap.put("logEncoding", "\${LOG_ENCODING}")
		}

		if (DSN != null) {
			for (String key : config.datasetMappings.keySet()) {
				if (DSN.endsWith(key)) {
					String newValue = config.datasetMappings.getProperty(key)
					// Remove variable syntax ${}
					String variableKey = newValue.substring(2, newValue.length()-1)
					config.addVariable(variableKey, DSN)
					DSN = newValue;
					break
				}
			}
			ddMap.put("dsn", DSN)
		}

		if (options != null) {
			ddMap.put("options", options)
		}
		//ddMap.put("input", false) find value in scanner output
		if ("SYSIN".equals(name)) {
			ddMap.put("input", true)
		}
		
		if (output != null && output) {
			ddMap.put("output", output)
		}

		if (pass != null && pass) {
			ddMap.put("pass", pass)
		}

		if (instreamData != null) {
			ddMap.put("instream", instreamData)
		}

		return ddMap
	}
}

class DD {
	String name
	ArrayList<DSN> DSNs	

	public List<Map<String, Object>> toYaml(Configuration config) {
		List<Map<String, Object>> ddList = new ArrayList<>()
		if (DSNs == null || DSNs.isEmpty()) {
			return ddList
		}

		if (name == null) {
			throw IllegalStateException("No name was set for this DD concatenation: " + DSNs.toString())
		}

		ddList.add(DSNs.get(0).toYaml(config, name))
		for (int i=1; i<DSNs.size(); i++) {
			ddList.add(DSNs.get(i).toYaml(config, null))
		}
		
		return ddList;
	}
}

class Step {
	// General fields
	String name // Find value in scanner output
	String type // Only supported typed are 'mvs' and 'job'
	Integer maxRC

	// MVS fields
	String program
	String parms
	ArrayList<DD> DDs

	// Job fields
	String jcl

	public Map<String, Object> toYaml(Configuration config) {
		Map<String, Object> stepMap = new LinkedHashMap<>();
		String name = this.name == null ? "<STEP_NAME>" : this.name

		if ("mvs".equals(type) == false && "job".equals(type) == false) {
			throw IllegalStateException("Unknown step type '$type' found. Step: $name Program: $program")
		}

		// General fields
		stepMap.put("step", name)
		stepMap.put("type", type)
		if (maxRC != null) {
			stepMap.put("maxRC", maxRC)
		}

		if ("mvs".equals(type)) {
			if (program == null) {
				throw new IllegalStateException("No program was specified for the step: " + toString())
			}
			stepMap.put("pgm", program)

			if (parms != null) {
				stepMap.put("parm", parms);
			}
			
			if (DDs != null) {
				List<Map<String, Object>> ddsList = new ArrayList<>()
				for (DD dd : DDs) {
					ddsList.addAll(dd.toYaml(config))
				}
				stepMap.put("dds", ddsList)
			}
		} else { // 'job' type
			List<Map<String, String>> logs = new ArrayList<>()
			Map<String, String> log = new LinkedHashMap<>()
			logs.add(log)
			log.put("log", "\${LOGS}/\${STEP}-\${FILE_NAME}.log")
			log.put("ddname", "*")
			log.put("logEncoding", "\${LOG_ENCODING}")
			
			stepMap.put("logs", logs)
			stepMap.put("text", jcl)
		}

		return stepMap;
	}

	public String toString() {
		return String.format("Step '%s': type='%s', program='%s', maxRC='%d', parms='%s', dds='%s'", name, type, program, maxRC, parms, DDs)
	}
}

class Configuration {
	private String tempLangName = "<LANG_NAME>"
	private String tempSources1 = "<File_Patterns_Matching_USS_Source_Files>"
	private String tempSources2 = "cobol/*.cbl"
	private String tempDatasetName1 = "STATIC.DSN.NAME"
	private String tempDatasetName2 = "\${HLQ}.DYNAMIC.NAME"
	private String tempDatasetOptions1 = "<DATASET_OPTIONS>"
	private String tempDatasetOptions2 = "cyl space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library)"
	List<Map<String, Object>> variables = new ArrayList<>()
	Map<String, Object> yaml = new LinkedHashMap<>()
	Properties datasetMappings = new Properties();

	Configuration() {
		// Insert default structure into the yaml
		List<Map<String, Object>> tasks = new ArrayList<>()
		Map<String, Object> lang = new LinkedHashMap<>()
		lang.put("language", tempLangName)
		tasks.add(lang)
		
		List<String> sources = new ArrayList<>()
		sources.add(tempSources1)
		sources.add(tempSources2)
		lang.put("sources", sources)
		
		// Hardcoded
		List<Map<String, String>> datasets = new ArrayList<>()
		Map<String, String> dataset1 = new LinkedHashMap<>()
		dataset1.put("name", tempDatasetName1)
		dataset1.put("options", tempDatasetOptions1)
		Map<String, String> dataset2 = new LinkedHashMap<>()
		dataset2.put("name", tempDatasetName2)
		dataset2.put("options", tempDatasetOptions2)
		
		datasets.add(dataset1)
		datasets.add(dataset2)
		lang.put("datasets", datasets)
		
		lang.put("variables", variables)

		yaml.put("version", "1.0.0")
		yaml.put("tasks", tasks)
	}

	void addStep(Step step) {
		getSteps().add(step.toYaml(this))
	}

	Map<String, Object> getLanguage() {
		// Return the singular language task we're constructing
		for (Map<String, Object> task : yaml.get("tasks")) {
			if (tempLangName.equals(task.get("language"))) {
				return task;
			}
		}

		throw new IllegalStateException("No default language has been added, Configuration object not initialized properly")
	}

	List<Map<String, Object>> getSteps() {
		if (getLanguage().containsKey("steps") == false) {
			getLanguage().put("steps", new ArrayList<Map<String, Object>>())
		}

		return getLanguage().get("steps");
	}

	void addVariable(String key, Object value) {
		addVariable(key, value, false)
	}

	void addVariable(String key, Object value, boolean first) {
		for (Map<String, Object> variable : variables) {
			if (((String)variable.get("name")).equals(key)) {
				if (value.equals(variable.get("value")) == false) {
					println("A variable for key '$key' already exists. The value '$value' differs from the existing value: '${variable.get('value')}'. If the different value is needed, update it in the yaml by hardcoding it or adding another variable.")
				}
				return
			}
		}
		Map<String, Object> variable = new LinkedHashMap<>()
		variable.put("name", key)
		variable.put("value", value)
		if (first) {
			variables.add(0, variable)
		} else {
			variables.add(variable)
		}
	}

	public Map<String, Object> toYaml() {
		return yaml;
	}
}



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
	
def configFolder = parameters.c
String configFileName = '../conf/jclmig.config'
String datasetMapName = '../conf/datasetMappings.properties'
if (configFolder) {
	if (configFolder.endsWith("/") == true) {
		configFolder = configFolder.substring(0, configFolder.length()-1)
	}
	configFileName = "$configFolder/jclmig.config"
	datasetMapName = "$configFolder/datasetMappings.properties"
}

def configFile     = scriptLocation.resolve(configFileName).toFile()
def datasetMapFile = scriptLocation.resolve(datasetMapName).toFile()
if (!configFile.exists())
{
	println "File $configFile does not exist. Need to specify a valid JCL migration config file"
	System.exit(1)
}

//******************************************************************************
//* Parses the JCL migration config file
//******************************************************************************
def config = new Properties()
try (DataInputStream stream = configFile.newDataInputStream()) {
	config.load(configFile.newDataInputStream())
}

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


//*********
//* YAML
//*********

def YAMLoutput = new YamlBuilder() {
	@Override
	public String toString() {
		return writeBlockStringYaml(new StringReader(JsonOutput.DEFAULT_GENERATOR.toJson(getContent())))
	}
}

Configuration configuration = new Configuration()

if (datasetMapFile.exists()) {
	try (DataInputStream stream = datasetMapFile.newDataInputStream()) {
		configuration.datasetMappings.load(stream)
	}
}
boolean printOnce = true
int stepCount = 0
steps.each { step ->
	stepCount++
	// The scanner contains a bug, preventing this name from being accurate. It doesn't seem to update past the first name grabbed
	println "Processing step $stepCount: ${step.name}"
	Step configstep = new Step()
	
	// General step config options
	configstep.name = step.name
	if (step.maxRC.isEmpty() == false) {
		configstep.maxRC = Integer.parseInt(step.maxRC)
	} else {
		configstep.maxRC = 8
	}

	String program = step.exec.name
	boolean isRestricted = restrictedPgms.contains(program)

	if (isRestricted) {
		println "WARNING: Program ${step.exec.name} may require special authority. The generated exec command may need to be modified."
		configstep.type = "job"
		def job = project.file.jcl.job
		// Define the JCL used in the JobExec step
		StringBuilder jcl = new StringBuilder()
		jcl.append(breakup("//${job.@name.text().padRight(8)} JOB ${job.jobcard.@data}"))
		jcl.append("\n")
		String pgm = "${step.exec.name}" + (step.parm.text().isEmpty() ? "" : ",PARM=${step.parm}")
		jcl.append(breakup("//${step.name.text().padRight(8)} EXEC PGM=${pgm}"))
		jcl.append("\n")
		step.dd.each { ddx ->
			def firstAllocation = ddx.concat.find{it.@sequence == "1"}
			jcl.append("//${ddx.name.text().padRight(8)} DD ${firstAllocation.parm}")
			jcl.append("\n")
			def ddm = convertAllocationToDD(firstAllocation, configuration)
			if (ddm.'instreamData') {
				def dlm = (ddm.'dlm') ? ddm.'dlm' : "/*"
				jcl.append("${ddm.'instreamData'}$dlm")
				jcl.append("\n")
			}
			
			ddx.concat.each { concat ->
				if (concat.@sequence != "1") {
					jcl.append("//${"".padRight(8)} DD ${concat.parm}")
					jcl.append("\n")
					ddm = convertAllocationToDD(concat, configuration)

					if (ddm.'instreamData') {
						def dlm = (ddm.'dlm') ? ddm.'dlm' : "/*"
						jcl.append("${ddm.'instreamData'}$dlm")
						jcl.append("\n")
					}
				}
			}
		}
		configstep.jcl = jcl.toString()
	} else {
		configstep.type = "mvs"
		configstep.program = program
		configstep.parms = step.parm.text().replaceAll(/^'/,"").replaceAll(/'$/,"")
		configstep.DDs = new ArrayList<DD>()
		step.dd.each { ddx ->
			DD newDD = new DD()
			newDD.DSNs = new ArrayList<DSN>() 
			newDD.name = ddx.name
			ddx.concat.each { concat ->
				newDD.DSNs.add(generateDSN(concat, configuration))
			}
			configstep.DDs.add(newDD)
		}
	}
	configuration.addStep(configstep)
}

Map<String, Object> yaml = configuration.toYaml()

YAMLoutput(yaml)
//println YAMLoutput.toString()
File yamlFile = new File(outputDir, "${member}.yaml");
try (FileOutputStream stream = new FileOutputStream(yamlFile);
		OutputStreamWriter writer = new OutputStreamWriter(stream, StandardCharsets.UTF_8)) {
	YAMLoutput.writeTo(writer)
}

char CCSID_UTF_8 = 1208;
FileAttribute.Tag tag = new FileAttribute.Tag( CCSID_UTF_8, true );
FileAttribute.setTag( yamlFile.getPath(), tag );

return

/******************
* Utility Methods *
*******************/

def convertAllocationToDD(def concat, Configuration configuration)
{
	dd = [:]
	def options = []
	isTemp = false
	if (!concat.dsn.text().isEmpty())
	{
		def dsn = "${concat.dsn}"
		if ( dsn.startsWith("&") && !dsn.startsWith("&&") )
		{
			println "WARNING: Parameter, $dsn, could not be resolved. A variable has been put in its place, please update its value or hardcode the DSN."
			dsn = dsn.substring(1, dsn.length())
			configuration.addVariable(dsn, "<PLACEHOLDER_VALUE>", true)
			dsn = "\${${dsn}}"
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

String breakup(String line)
{
	List<String> lines = []
	if (line.length() > 71)
	{
		ndx = line.substring(0,71).lastIndexOf(',')
		lines << line.substring(0,ndx+1)
		lines << breakup("//         ${line.substring(ndx+1)}")
	}
	else
		lines << line
	
	return lines.join('\n')
}

def generateDSN(def concat, Configuration configuration) {
	DSN newDSN = new DSN()
	def options = []
	isTemp = false
	if (!concat.dsn.text().isEmpty()) {
		def dsn = "${concat.dsn}"
		if (dsn.startsWith("&") && !dsn.startsWith("&&")) {
			println "WARNING: Parameter, $dsn, could not be resolved. A variable has been put in its place, please update its value or hardcode the DSN."
			dsn = dsn.substring(1, dsn.length())
			configuration.addVariable(dsn, "<PLACEHOLDER_VALUE>", true)
			dsn = "\${${dsn}}"
		}
		newDSN.DSN = dsn
	}
	if (!concat.stat.text().isEmpty()) {
		options << "${concat.stat}".toLowerCase()
	}
	if (!concat.dispnor.text().isEmpty()) {
		if (concat.dispnor.text() == "PASS")
			newDSN.pass = true
		else {
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
	if (!concat.parm.text().isEmpty()) {
		def parms = concat.parm.text()
		def parmlist = splitit(parms, ",")
		parmlist.each { parm ->
			if (parm.startsWith("DSN")) {} // ignore, already handled above
			else if (parm.startsWith("DISP")) {} // ignore, already handled with stat and dispnor above
			else if (parm.startsWith("DLM")) {
				def value = parm.substring( "DLM=".length() )
				//newDSN.DLM = value
			}
			else if (parm == "*" || parm == "DATA")  { // in stream data
				data = ""
				concat.data.each { line ->
					data += (line.text().length()<=72)?line.text():line.text().substring(0,72)
					data += "\n"
				}
				newDSN.instreamData= data
			}
			else if (parm.startsWith("DDNAME=")) {
				def m = parms =~ /(.*)=(.*)/;
				def ddName = m[0][2]
				newDSN.DSN = ddName
			}
			else if (parm.startsWith("DCB=")) {
				def value = parm.substring( "DCB=".length() )
				value = value.trim().replaceFirst("\\(", "").replaceAll("\\)\$", "")
				def dcblist = splitit(value, ",")
				dcblist.each { dcbparm ->
					options << processAllocOption(dcbparm)
				}
			}
			else if (parm.startsWith("VOL")) {
				def value = parm.substring(parm.indexOf("=") + 1)
				options << processVolumeOption(value)
			}
			else if (parm.startsWith("UNIT")) {
				def value = parm.substring(parm.indexOf("=") + 1)
				options << processUnitOption(value)
			}
			else if (parm.startsWith("LABEL")) {
				def value = parm.substring(parm.indexOf("=") + 1)
				options << processLabelOption(value)
			}
			else if (parm == "DUMMY" || parm.startsWith("SYSOUT=")) {
				isTemp = true
				newDSN.output = true
			}
			else {
				options << processAllocOption(parm)
			}
		}
	}
	if (!options.isEmpty())
		newDSN.options = options.join(' ')
	else if (isTemp)
		newDSN.options = tempCreateOptions
	return newDSN
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

String writeBlockStringYaml(Reader jsonReader) {
	try (Reader reader = jsonReader) {
		JsonNode json = new ObjectMapper().readTree(reader)

		YAMLMapper mapper = new YAMLMapper()
		mapper.configure(YAMLGenerator.Feature.LITERAL_BLOCK_STYLE, true)
		return mapper.writeValueAsString(json)
	} catch (IOException e) {
		throw new YamlRuntimeException(e)
	}
}


//
//   Parse the command line arguments
//
def parseArgs(String[] args) {
	String usage = 'JCLtoDBB.groovy [options]'
	
	def cli = new CliBuilder(usage:usage)
		  cli.c(longOpt: 'configFolder',    args:1, argName: 'configFolder',                optionalArg:false,  'Path to the config folder containing the JCL migration configuration file and the dataset mappings configuration file.  If specified, path is considered absolute if it begins with a slash else it is relative path from the migration tool bin directory.  Default is ../conf/.')
		  cli.d(longOpt: 'dataset',       args:1, argName: 'MVS dataset',                   optionalArg:false, 'Dataset containing JCL to be migrated (Required)')
		  //cli.g(longOpt: 'genExecVars',   args:1, argName: 'Generate executable variables', optionalArg:true,  'Specify true to generate executable variables')
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
