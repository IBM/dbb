@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript

/*
* This step task Groovy script checks for existence of a given absolute
* file path on z/OS Unix provided by config variable 'checkFileExistsPath'.
* The output is a new boolean variable provided by config variable
* defined by the 'checkFileExistsVariableName'.
*/

// get checkFileExistsPath variable from pre-defined language config object
String filePath = config.getVariable("checkFileExistsPath")
if (filePath == null) {
   println ">> ERROR: checkFileExistsPath variable for is not set"
   return -1
}

// get file exists variable name to set. 
String variableName = config.getVariable("checkFileExistsVariableName")
if (variableName == null) {
   println ">> ERROR: checkFileExistsVariableName variable for is not set"
   return -1

// Create new File object 
def file = new File(filePath)

println "> Checking for file existence of '${filePath}'"

// Check if file exists
if (file.exists()) {
    config.setVariable(variableName, true)
    println "> Setting variable '${variableName}' to 'true'"
} else {
    config.setVariable(variableName, false)
    println "> Setting variable '${variableName}' to 'false'"
}

return 0