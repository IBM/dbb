@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.build.*
import groovy.transform.*
import groovy.cli.commons.*

// global variables
@Field BuildProperties properties = BuildProperties.getInstance()
@Field RepositoryClient repoClient = null
@Field def groups = []
@Field def collections = []

// run setup tasks
setup(args)

// process build group list
println "** Deleting build groups: $groups"
groups.each { group ->
  String name = group.trim()
  repoClient.deleteBuildResults(name)
  println "*** Deleting build group '$name' -> Status = ${repoClient.getLastStatus()}"
}

// process collections list
println "** Deleting collections: $collections"
collections.each { collection ->
  String name = collection.trim()
  repoClient.deleteCollection(name)
  println "*** Deleting collection '$name' -> Status = ${repoClient.getLastStatus()}"
}

// end script

/*
  setup :
  handle cli arguments, load property file if present, 
  create repository client, populate deletion lists
*/
def setup(String[] args) {

    // parse input arguments
	String usage = 'WebAppCleanUp.groovy [options]'
	String header = 'options:'
	
	def cli = new CliBuilder(usage:usage,header:header)
	cli.g(longOpt:'groups', args:1, 'Comma separated list of Build Groups to delete')
	cli.c(longOpt:'collections', args:1, 'Comma separated list of Collections to delete')
	cli.G(longOpt:'groupsFile', args:1, 'Absolute or relative path (from script directory) of file containing build groups to delete')
	cli.C(longOpt:'collectionsFile', args:1, 'Absolute or relative path (from script directory) of file containing collections to delete')
	
	// web application credentials (overrides properties in WebAppCleanUp.properties)
	cli.u(longOpt:'url', args:1, 'DBB WebApp URL')
	cli.i(longOpt:'id', args:1, 'DBB WebApp ID')
	cli.p(longOpt:'pw', args:1,  'DBB WebApp Password')
	cli.P(longOpt:'pwFile', args:1, 'Absolute or relative (from this script) path to file containing DBB password')
	cli.prop(longOpt:'propertyFile', args:1, 'Absolute or relative (from this script) path to property file that contains DBB WebApp information (Optional)')	

		cli.h(longOpt:'help', 'Prints this message')
	def opts = cli.parse(args)
	if (!args || !opts) {
	    cli.usage()
		System.exit(1)
	}
	
	// if help option used, print usage and exit
    if (opts.h) {
		cli.usage()
		System.exit(0)
	}

	// if specified, load user properties file
	if (opts.prop) {
		String filePath = opts.prop
		if (!filePath.trim().startsWith('/'))
			filePath = "${getScriptDir()}/$filePath"
		properties.load(new File(filePath))
	}

   	// update authentication properties with cli options
   	if (opts.u) properties.url = opts.url
   	if (opts.i) properties.id = opts.id
   	if (opts.p) properties.pw = opts.pw
   	if (opts.P) properties.pwFile = opts.pwFile
   	
   	// validate authentication properties
   	assert properties.url : "Missing 'url' argument/property"
   	assert properties.id : "Missing 'id' argument/property"
   	if (!(properties.pw || properties.pwFile)) {
   		assert properties.pw : "Missing 'pw' argument/property"
   		assert properties.pwFile : "Missing 'pwFile' argument/property"
   	}
   	
   	//create repository client
   	println "Creating repository client for ${properties.url}"
   	repoClient = new RepositoryClient().forceSSLTrusted(true).url(properties.url).userId(properties.id)
   	if (properties.pw)
   	   repoClient.setPassword(properties.pw)
	else {
   	   String filePath = properties.pwFile
	   if (!filePath.trim().startsWith('/'))
	      filePath = "${getScriptDir()}/$filePath"
	   repoClient.setPasswordFile(new File(filePath))
	}
   	
	// populate build groups list
	if (opts.g)
	   groups.addAll(opts.g.split(','))
	   
	if (opts.G) {
		String filePath = opts.G	
		if (!filePath.trim().startsWith('/'))
		   filePath = "${getScriptDir()}/$filePath"
		def GList = []
		new File(filePath).eachLine { line ->
		   GList << line
		}
		groups.addAll(GList)	
	}
   	
	// populate collections list
	if (opts.c) 
	   collections.addAll(opts.c.split(','))
	   
	if (opts.C) {
		String filePath = opts.C
		if (!filePath.trim().startsWith('/'))
		   filePath = "${getScriptDir()}/$filePath"
		def CList = []
		new File(filePath).eachLine { line ->
		   CList << line
		}
		collections.addAll(CList)	
	}  

}
