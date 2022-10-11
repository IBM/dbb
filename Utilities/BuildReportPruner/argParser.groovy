import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import groovy.cli.commons.*

user = "None"

def parseArgs(String[] cliArgs){
	def cli = new CliBuilder(usage: "Pruner.groovy [options] <preview>",header: "\nAvailable options\n")
	cli.h(longOpt:'help','help')
	cli.u(longOpt:'unsave','unsave')
	cli.a(longOpt:'all', 'Deletes all builds in all groups')
	cli.o(longOpt: 'oldest',args:1,optionalArg:true,argName:'numToKeep','Number of most recent builds to skip when pruning')
	cli.g(longOpt: 'groupName', args:1,optionalArg:true, argName:'groupName','Specify a group name')
	cli.l(longOpt: 'labelName',args:1, optionalArg:true,argName:'labelName','Specify a label name')
	cli.c(longOpt: 'cleanBuildsToKeep',args:1,optionalArg:true,argName:'cleanBuilds', 'Number of clean builds to keep from pruning')
	cli.e(longOpt: 'errorBuildsToKeep',args:1,optionalArg:true, argName:'errorBuilds','Number of error builds to keep from pruning')
	cli.w(longOpt: 'warningBuildsToKeep',args:1,optionalArg:true, argName:'warningBuilds','Number of warning builds to keep from pruning')
	cli.i(longOpt: 'interactive','Toggle the interactive mode')
	cli.p(longOpt:'preview','Preview tag')
	
	def opts = cli.parse(cliArgs)
	if (opts.h) {
		printHelp()
		System.exit(0)
	}
	
	if (opts.g && opts.g instanceof Boolean) {
		println("-g needs you to specify a group name. Run with -h to view details.")
		System.exit(0)
	}
	
	if (opts.l && opts.l instanceof Boolean) {
		println("-l needs you to specify a label name. Run with -h to view details.")
		System.exit(0)
	}
	
	if (opts.o) {
		if ((opts.c && opts.c instanceof Boolean) || (opts.e && opts.e instanceof Boolean) || (opts.w && opts.w instanceof Boolean)) {
			println("-c,-e,and -w need you to specify the number of builds to keep. Run with -h to view details.")
			System.exit(0)
		}
		if (!opts.c&&!opts.e&&!opts.w&&opts.o instanceof Boolean) {
			println("Sorry, please specify the number of builds to keep for -o. Run with -h to view details.")
			System.exit(0)
		}
	}
	
	if (opts.u) {
		user = "Unsave"
	}
	return opts
}


def setUpStore() {
	def properties = BuildProperties.getInstance()
	def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
	def buildPropFile = new File("$scriptDir/user.properties")
	if (buildPropFile.exists()) {
		BuildProperties.load(buildPropFile)
	}

	if (properties.db2 == "true") {
		if (!properties.url || !properties.userId) {
			println("You must update the user.properties file first to be able to use the Pruner")
			System.exit(0)
		}

		try {
			if (properties.password != null) {
				return MetadataStoreFactory.createDb2MetadataStore(properties.url, properties.userId, properties.password)
			}
			else if (properties.passwordFile != null) {
				return MetadataStoreFactory.createDb2MetadataStore(properties.url, properties.userId, new File(properties.passwordFile))
			}
			else {
				println("Please enter either a password or the path for a password file in user.properties")
				System.exit(0)
			}
		}
		catch (BuildException e) {
			println("Faled to instantiate Db2 Metadata Store: " + e.getMessage())
		}
	}
	else {
		return MetadataStoreFactory.createFileMetadataStore(properties.dir)
	}
}

def printHelp() {
	def message = (user.equals("Prune")) ? ", the rest will be pruned" : ""
	
	println()
	println("*****************"+user+"r.groovy*****************")
	println("Options so far:")
	if (!user.equals("Prune")) {
		println("-u,--unsave")
		println(": will turn off the save flag for the specified build(s)")
	}
	println("-a,--all")
	println(": will " + user.toLowerCase() + " all builds in all groups")
	println()
	println("-g,--group <group name>")
	println(": will " + user.toLowerCase() + " all the builds in the specified group")
	println("Optional specifiers:")
	println("-c,--cleanBuildsToKeep")
	println(": will " + user.toLowerCase() + " all clean builds in the specified group")
	println("-e,--errorBuildsToKeep")
	println(": will " + user.toLowerCase() + " all error builds in the specified group")
	println("-w,--warningBuildsToKeep")
	println(": will " + user.toLowerCase() + " all warning builds in the specified group")
	println("-l,--labelName <label name>")
	println(": will " + user.toLowerCase() + " the build in the specified group with the specified label")
	println()
	println("-o,--oldest <number of builds to keep>")
	println(":will save the most recent specified number of builds" + message)
	println("Optional specifiers:")
	println("-g,--group <group name>")
	println(": will limit " + user.toLowerCase() + " to the specified group")
	println("-c,--cleanBuildsToKeep <number of clean builds to keep>")
	println(":will save the most recent specified number of clean builds" + message)
	println("-e,--errorBuildsToKeep <number of error builds to keep>")
	println(":will save the most recent specified number of error builds" + message)
	println("-w,--warningBuildsToKeep <number of warning builds to keep>")
	println(":will save the most recent specified number of warning builds" + message)
	println("Note that if -c, -e, or -w is used, then no number needs to be entered with the -o option")
	println()
	println("-i,--interactive")
	println(": will launch an interactive mode with the same possible options as above")
	println("To test the result of any option and not make any changes, add -p/--preview.")
}
