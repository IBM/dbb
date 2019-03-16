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
client = tools.setUpClient("Save")
def opts = tools.parseArgs(args)
preview = false
if(opts.p){
	preview = true
}

//will save on default, user can enter -u to toggle unsave
state = "yes"
if(opts.u){
	state="no"
}

//methods

def checkGroupName(String name){
	//Will check if the entered group name exists
	def groups = client.listBuildResultGroups()
	if(!groups.contains(name)){
		println("Sorry, that group does not exist. Check the spelling and try again.")
		System.exit(0)
	}
}

def saveBR(buildRes,state){
	//Will toggle the save flag for the specified build
	if(buildRes == null){
		println("Sorry, that build does not exist. Check spelling and try again.")
		System.exit(0)
	}
	if(preview){
		println("This is a preview:")
	}else{
		buildRes.setProperty("save",state)
		buildRes.save()
	}
	if(state.equals("yes")){
		println("Build: " + buildRes.getLabel() + " will now be saved")
	}else{
		println("Build: " + buildRes.getLabel() + " will now not be saved")
	}
	println()
}

def saveBuilds(currGroup, numToKeep = currGroup.size()){
	//Will toggle the save flag for the builds (or the number of specified builds) in the specified group.
	def message = (state.equals("yes")) ? "Not being saved: " : "Not being unsaved: "
	def i = 0
	currGroup.each{ buildResult->
		if(i<numToKeep){
			saveBR(buildResult,state)
			i++
		}else{
			println(message + buildResult.getLabel())
		}
	}
}

//end methods



if(opts.a){
	//Will toggle the save flag for all builds
	def builds = client.getAllBuildResults(new HashMap<String,String>())
	builds.each{ buildResult ->
		saveBR(buildResult,state)
	}
	
}else if(opts.d){
	//Will toggle the save flag for the build with the specified ID
	def buildResult = client.getBuildResult((long)opts.d.toInteger())
	saveBR(buildResult,state)
	
}else if(opts.o){
	//Will toggle the save flag for a specified amount of the most recent builds 
	def queryParms = ["orderBy":"lastUpdated","order":"DESC"]
	if(opts.g){
		//checking if a group has been specified
		def groupName = opts.g
		checkGroupName(groupName)
		queryParms.put("group",groupName)
	}
	
	def message = (state.equals("yes")) ? "Now saving " : "Now unsaving "
	
	if(opts.c||opts.e||opts.w){
		//checking if any states has been specified
		if(opts.c){
			println(message+ "clean builds:")
			queryParms.put("status","0")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup,opts.c.toInteger())
		}
		if(opts.e){
			println(message + "error builds:")
			queryParms.put("status","1")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup,opts.e.toInteger())
		}
		if(opts.w){
			println(message + "warning builds:")
			queryParms.put("status","2")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup,opts.w.toInteger())
		}
		
	}else{
		def currGroup = client.getAllBuildResults(queryParms)
		saveBuilds(currGroup,opts.o.toInteger())
	}
	
}else if(opts.g){
	def groupName = opts.g
	checkGroupName(groupName)
	
	if(opts.l){
		//checking if a label has been specified
		def buildResult = client.getBuildResult(groupName, opts.l)
		saveBR(buildResult,state)
		System.exit(0)
	}
	
	def queryParms = ["group":groupName,"orderBy":"lastUpdated","order":"DESC"]
	if(opts.c||opts.e||opts.w){
		def message = (state.equals("yes")) ? "Now saving " : "Now unsaving "
		//checking if a state has been specified
		if(opts.c){
			println(message + "clean builds: ")
			queryParms.put("status","0")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup)
		}
		if(opts.e){
			println(message + "error builds:")
			queryParms.put("status","1")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup)
		}
		if(opts.w){
			println(message + "warning builds:")
			queryParms.put("status","2")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup)
		}
	}else{
		def currGroup = client.getAllBuildResults(queryParms)
		saveBuilds(currGroup)
	}
	
	
}else if(opts.i){
	def check = System.console().readLine("Save or unsave? (save/unsave): ")
	option = System.console().readLine("Enter which option you'd like to initiate: ")
	if(check.equals("unsave")) state = "no"
	def message = (state.equals("yes")) ? "Now saving " : "Now unsaving "
	
	
	if(option.equals("-a")||option.equals("--all")){
		def selection = System.console().readLine "Are you sure you want to $check all builds in all groups? (yes/no): "
		
		
		if(selection.equals("yes")||selection.equals("YES")||selection.equals("Yes")||selection.equals("y")){
			def builds = client.getAllBuildResults(new HashMap<String,String>())
			builds.each{ buildResult ->
				saveBR(buildResult,state)
			}
		}else{
			println("Not changing build results.")
		}
		
		
	}else if(option.equals("-d")||option.equals("--id")){
		def id = System.console().readLine "Specify the ID of the build you'd like to $check: "
		def buildResult = client.getBuildResult((long)id)
		saveBR(buildResult,state)
		
	}else if(option.equals("-g")||option.equals("--group")){
		
		def groupName = System.console().readLine "Specify Group Name: "
		checkGroupName(groupName)
		
		def selection = System.console().readLine "Would you like to specify a label name?(yes/no) "
		if(selection.equals("yes")||selection.equals("YES")||selection.equals("Yes")||selection.equals("y")){
			def labelName = System.console().readLine "Specify a label name: "
			def buildResult = client.getBuildResult(groupName, labelName)
			saveBR(buildResult,state)
			System.exit(0)
		}
		
		def queryParms = ["group":groupName]
		def selection2 = System.console().readLine "Would you like to specify a specific state to $check? (yes/no)"
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
			saveBR(buildResult,state)
		}
	}else if(option.equals("-o")||option.equals("--oldest")){
		
		def strCleanToKeep = System.console().readLine "Please specify the amount of clean builds to $check.\nEnter -1 to ignore clean build results: "
		def strErrorToKeep = System.console().readLine "Please specify the amount of error builds to $check.\nEnter -1 to ignore error build results: "
		def strWarningToKeep = System.console().readLine "Please specify the amount of warning builds to $check.\nEnter -1 to ignore warning build results: "
	
		if(!strCleanToKeep.isNumber()||!strErrorToKeep.isNumber()||!strWarningToKeep.isNumber()){
			println("Please only provide numbers for the amounts of builds you'd like to $check")
			System.exit(0)
		}
		
		def numCleanToKeep = strCleanToKeep.toInteger()
		def numErrorToKeep = strErrorToKeep.toInteger()
		def numWarningToKeep = strWarningToKeep.toInteger()
		
		def queryParms = ["orderBy":"lastUpdated","order":"DESC"]
		
		def selection = System.console().readLine "Would you like to specify a specific group to $check? (yes/no): "
		if(selection.equals("yes")||selection.equals("YES")||selection.equals("Yes")||selection.equals("y")){
			
			def groupName = System.console().readLine "Specify Group Name: "
			checkGroupName(groupName)
			queryParms.put("group",groupName)
		}
			
		if(numCleanToKeep != -1){
			println(message + "clean builds:")
			queryParms.put("status","0")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup,numCleanToKeep)
		}
		if(numErrorToKeep != -1){
			println(message + "error builds:")
			queryParms.put("status","1")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup,numErrorToKeep)
		}
		if(numWarningToKeep != -1){
			println(message + "warning builds:")
			queryParms.put("status","2")
			def currGroup = client.getAllBuildResults(queryParms)
			saveBuilds(currGroup,numWarningToKeep)
		}
	
	}else{
		println("Sorry, please enter a recognized parameter. Run again with -h or --help to see all the options")
		System.exit(0)
	}
	
}else{
	println("Sorry, please enter a recognized parameter for Saver.groovy. Run again with -h or --help to see all the options")
	System.exit(0)
}
