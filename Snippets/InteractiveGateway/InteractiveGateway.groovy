@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.build.*

// load interactive gateway configuration properties
def properties = BuildProperties.getInstance()
def propFile = "${getScriptDir()}/gateway.properties"
properties.load(new File(propFile))
validate(propFile)

println "Create a simple Hello Rexx script to execute"
new File(properties.workDir).mkdirs()
def helloRexx = new File("${properties.workDir}/hello.rexx")

// HELLO Rexx script keeps prompting until you say 'STOP!'
helloRexx << """/* REXX */

name = ''
say 'What is your name?'
do until name == 'STOP!'
   pull name
   if name /== 'STOP!' then
	 do
	   say 'Hello' name
	   say 'What is your name?'
	 end
end
say 'Fine be that way!'
"""

println "Create a dataset and copy HELLO Rexx script to the dataset for execution"
def rexxPDS = "${properties.hlq}.REXX"
new CreatePDS().dataset(rexxPDS).create()
new CopyToPDS().file(helloRexx).dataset(rexxPDS).copy()

println "Run HELLO Rexx script using the interactive gateway..."
println ""
def hello = new TSOExec().command("EXEC '$rexxPDS(HELLO)'")
hello.logFile(new File("${properties.workDir}/gateway.log")).logEncoding("ibm1047")
def rc = hello.execute()
println hello.getOutput().trim()
	
["John", "Paul", "George", "Ringo", "STOP!"].each { name ->
   if (rc == 0 && hello.isWaitingForResponse()) {
	   println name
	   rc = hello.response(name).execute()
	   println hello.getOutput().trim()
   }
   else if (rc != 0)
	   println "ERROR rc=$rc"
}

// verify that the gateway.properties file has been updated with required configuration information
def validate(String propFile) {
	def properties = BuildProperties.getInstance()
	assert properties.workDir : "Missing property 'workDir'. Please edit $propFile"
	assert properties.hlq : "Missing property 'hlq'. Please edit $propFile"
	assert properties.'dbb.gateway.procedureName' : "Missing property 'dbb.gateway.procedureName'. Please edit $propFile"
	assert properties.'dbb.gateway.accountNumber' : "Missing property 'dbb.gateway.accountNumber'. Please edit $propFile"
	assert properties.'dbb.gateway.groupId' : "Missing property 'dbb.gateway.groupId'. Please edit $propFile"
	assert properties.'dbb.gateway.regionSize' : "Missing property 'dbb.gateway.regionSize'. Please edit $propFile"
}
