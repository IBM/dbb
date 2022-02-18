@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.cli.commons.*


def usage = "Mappings.groovy [options]"
def opts = parseArgs(args, usage)
def properties = loadProperties(opts)

def mapping = new PropertyMappings("datasetMapping")
println(" Print values of the key : ${mapping.getKeyName()}")

//Print the values of this file
mapping.getValues().each{value->
	println(value)
}
/*********************************************************************/
def copyOrder = ["BMS", "COBOL", "COPY", "LNK"]

def prefix = "srcFiles"
def files = new File("${getScriptDir()}/files.txt") as List<String>

println("\nPrint all files in the original order")
files.each{file->
	println(file)
}

copyOrder.each { folder ->
	// Use the PropertyMappings class to get the files mapped
	println("\nCopy the files into the dataset $folder")
	def copyFiles = mapping.getMappedList(folder, files)
	def fileName = "${properties.sourceDir}/${folder}"
	copyFiles.each { file ->
		copyToDataset("$prefix/$file",folder)
	}
}

println("\nLook if the file is mapped to the dataset COBOL")
files.each{file->
	if(mapping.isMapped("COBOL",file)){
		println("The file $file is mapped to the dataset COBOL")
	}
}

/**********************************************************************/
/* Copying the file associated with a given filePath to the           */
/* associated data set that is mapped to its parent, see              */
/* datasetMappings.properties                                         */
/**********************************************************************/

def copyToDataset(def filePath,def folder)
{
	def properties = BuildProperties.getInstance()
	def file = new File(filePath)
	def member = "${file.name.take(file.name.lastIndexOf('.'))?:file.name}".toUpperCase()
	def absFile = file.absolute ? file : new File("${properties.sourceDir}/$filePath")
	println "Copying file $absFile to ${properties.hlq}.${folder}($member)"
	new CopyToPDS().file(absFile).dataset("${properties.hlq}.${folder}").member(member).execute()
}

def parseArgs(String[] cliArgs, String usage) {
	def cli = new CliBuilder(usage: usage)
	cli.s(longOpt:'sourceDir', args:1, argName:'dir', 'Absolute path to source directory')
	cli.q(longOpt:'hlq', args:1, argName:'hlq', 'High level qualifier for partition data sets')
	def opts = cli.parse(cliArgs)
	return opts
}

def loadProperties(OptionAccessor opts) {
	// check to see if there is a ./build.properties to load
	def properties = BuildProperties.getInstance()

	// set command line arguments
	if (opts.s) properties.sourceDir = opts.s
	if (opts.q) properties.hlq = opts.q
	thefile = new File("${getScriptDir()}/mapping.properties")
	properties.load(thefile)

	println("** Build properties at startup:")
	println(properties.list())

	return properties
}