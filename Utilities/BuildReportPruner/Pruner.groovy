import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*

//checking that the user has inputed some parameter
if(args.length == 0){
	println("Please add an argument to the script. Run again with -h or --help to view options.")
	System.exit(0)
}


def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
File sourceFile = new File("$scriptDir/argParser.groovy")
Class groovyClass = new GroovyClassLoader(getClass().getClassLoader()).parseClass(sourceFile)
GroovyObject tools = (GroovyObject) groovyClass.newInstance()
client = tools.setUpClient("Prune")
def opts = tools.parseArgs(args)
preview = false
if(opts.p){
	preview = true
}


//methods
//will check that the entered group name exists
def checkGroupName(String name){
	def groups = client.listBuildResultGroups()
	if(!groups.contains(name)){
		println("Sorry, that group does not exist. Check the spelling and try again.")
		System.exit(0)
	}
}

//Will delete a build if it is not saved and the -p flag has not been set
def deleteBR(buildRes){
	if(buildRes == null){
		println("Sorry, that build does not exist. Check spelling and try again.")
		System.exit(0)
	}
	if(preview){
		println("This is a preview:")
	}
	if(!buildRes.getProperty("save").equals("yes")){
		println(buildRes.getGroup()+ "." + buildRes.getLabel() + " will be deleted.")
		if(!preview){
			client.deleteBuildResult(buildRes)
		}
	}else{
		println(buildRes.getGroup()+ "." + buildRes.getLabel() + " is saved, so it will not be deleted.")
	}
	println()
}

//will delete the builds in a group, optionally keep a specified aamount of builds
def deleteBuilds(currGroup, numToKeep = 0){
	def i = 0
	currGroup.each{ buildResult->
		if(i<numToKeep){
			println("Not Being Deleted: " + buildResult.getLabel())
			println()
			i++
		}else{
			deleteBR(buildResult)
		}
	}
}
//end methods


if(opts.a){
	//Will delete all builds
	def builds = client.getAllBuildResults(new HashMap<String,String>())
	builds.each{ buildResult ->
		deleteBR(buildResult)
	}
		
}else if (opts.d){
	//Will delete a build with the specified ID
	def buildResult = client.getBuildResult((long)opts.d.toInteger())
	deleteBR(buildResult)
	
}else if(opts.o){
	//Will delete builds, saving a specified amount of the most recent builds
	def queryParms = ["orderBy":"lastUpdated","order":"DESC"]
	if(opts.g){
		//checking if a group has been specified
		def groupName = opts.g
		checkGroupName(groupName)
		queryParms.put("group",groupName)
	}
	
	if(opts.c||opts.e||opts.w){
		//checking if any states have been specified
		if(opts.c){
			println("Now deleting clean builds:")
			queryParms.put("status","0")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup,opts.c.toInteger())
		}
		if(opts.e){
			println("Now deleting error builds:")
			queryParms.put("status","1")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup,opts.e.toInteger())
		}
		if(opts.w){
			println("Now deleting warning builds:")
			queryParms.put("status","2")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup,opts.w.toInteger())
		}
		
	}else{
		def currGroup = client.getAllBuildResults(queryParms)
		deleteBuilds(currGroup,opts.o.toInteger())
	}
	
}else if(opts.g){
	def groupName = opts.g
	checkGroupName(groupName)
	
	if(opts.l){
		//checking if a label has been specified
		def buildResult = client.getBuildResult(groupName, opts.l)
		deleteBR(buildResult)
		System.exit(0)
	}
	
	
	def queryParms = ["group":groupName,"orderBy":"lastUpdated","order":"DESC"]
	if(opts.c||opts.e||opts.w){
		//checking if a state has been specified
		if(opts.c){
			println("Now deleting clean builds:")
			queryParms.put("status","0")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup)
		}
		if(opts.e){
			println("Now deleting error builds:")
			queryParms.put("status","1")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup)
		}
		if(opts.w){
			println("Now deleting warning builds:")
			queryParms.put("status","2")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup)
		}
	}else{
		def currGroup = client.getAllBuildResults(queryParms)
		deleteBuilds(currGroup)
	}
}else if(opts.i){
	option = System.console().readLine("Enter which option you'd like to initiate: ")
	
	if(option.equals("-a")||option.equals("--all")){
		def selection = System.console().readLine "Are you sure you want to delete all builds in all groups? (yes/no): "
		
		if(selection.equals("yes")||selection.equals("YES")||selection.equals("Yes")||selection.equals("y")){
			def builds = client.getAllBuildResults(new HashMap<String,String>())
			builds.each{ buildResult ->
				deleteBR(buildResult)
			}
		}else{
			println("Not deleting all results.")
		}
		
		
	}else if(option.equals("-d")||option.equals("--id")){
		def id = System.console().readLine "Specify the ID of the build you'd like to delete: "
		def buildResult = client.getBuildResult((long)id.toInteger())
		deleteBR(buildResult,state)
		
	}else if(option.equals("-g")||option.equals("--group")){
		
		def groupName = System.console().readLine "Specify Group Name: "
		checkGroupName(groupName)
		
		def selection = System.console().readLine "Would you like to specify a label name?(yes/no) "
		if(selection.equals("yes")||selection.equals("YES")||selection.equals("Yes")||selection.equals("y")){
			def labelName = System.console().readLine "Specify a label name: "
			def buildResult = client.getBuildResult(groupName, labelName)
			deleteBR(buildResult)
			System.exit(0)
		}
		
		def queryParms = ["group":groupName]
		def selection2 = System.console().readLine "Would you like to specify a specific state to save? (yes/no)"
		if(selection2.equals("yes")||selection2.equals("YES")||selection2.equals("Yes")||selection2.equals("y")){
			def statusIn = System.console().readLine "Please specify a state: (clean/error/warning)"
			def status = ""
			if(statusIn.equals("clean")||statusIn.equals("CLEAN")) status = "0"
			if(statusIn.equals("error")||statusIn.equals("ERROR")) status = "1"
			if(statusIn.equals("warning")||statusIn.equals("WARNING")) status = "2"
			if(status.equals("")){
				println("Please enter a valid status: either clean, error, or warning")
				System.exit(0)
			}else{
				queryParms.put("status",status)
			}
		}
		
		def currGroup = client.getAllBuildResults(queryParms)
		currGroup.each{ buildResult ->
			deleteBR(buildResult)
		}
	}else if(option.equals("-o")||option.equals("--oldest")){
	
		def strCleanToKeep = System.console().readLine "Please specify the amount of clean builds to keep.\nEnter -1 to keep all clean build results: "
		def strErrorToKeep = System.console().readLine "Please specify the amount of error builds to keep.\nEnter -1 to keep all error build results: "
		def strWarningToKeep = System.console().readLine "Please specify the amount of warning builds to keep.\nEnter -1 to keep all warning build results: "
	
		if(!strCleanToKeep.isNumber()||!strErrorToKeep.isNumber()||!strWarningToKeep.isNumber()){
			println("Please only provide numbers for the amounts of builds you'd like to keep")
			System.exit(0)
		}
		
		def numCleanToKeep = strCleanToKeep.toInteger()
		def numErrorToKeep = strErrorToKeep.toInteger()
		def numWarningToKeep = strWarningToKeep.toInteger()
		
		def queryParms = ["orderBy":"lastUpdated","order":"DESC"]
		
		def selection = System.console().readLine "Would you like to specify a specific group to prune? (yes/no): "
		if(selection.equals("yes")||selection.equals("YES")||selection.equals("Yes")||selection.equals("y")){
			
			def groupName = System.console().readLine "Specify Group Name: "
			checkGroupName(groupName)
			queryParms.put("group",groupName)
		}
			
		if(numCleanToKeep != -1){
			println("Now deleting clean builds:")
			queryParms.put("status","0")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup,numCleanToKeep)
		}
		if(numErrorToKeep != -1){
			println("Now deleting error builds:")
			queryParms.put("status","1")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup,numErrorToKeep)
		}
		if(numWarningToKeep != -1){
			println("Now deleting warning builds:")
			queryParms.put("status","2")
			def currGroup = client.getAllBuildResults(queryParms)
			deleteBuilds(currGroup,numWarningToKeep)
		}
	
	}else{
		println("Sorry, please enter a recognized parameter. Run again with -h or --help to see all the options")
		System.exit(0)
	}
	
}else{
	println("Sorry, please enter a recognized parameter for Pruner.groovy. Run again with -h or --help to see all the options")
	System.exit(0)
}