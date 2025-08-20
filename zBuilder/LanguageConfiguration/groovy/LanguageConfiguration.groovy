@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript

import groovy.yaml.YamlSlurper

/*
* This simple YAML parser for zBuilder file configuration YAML files
* expects the following YAML format:
*
* config:  
* - name: compileParms
*   value: CICS,SQL
* - name: linkEditParms
*   value: RENT
*/

println "> Running LanguageConfig.groovy step task"

// get language variables from pre-defined language config object
String buildFile = config.getVariable("FILE")

// get the configSource as a file variable to support file configuration group files
String configSource = config.getFileVariable("configSource", buildFile)
if (configSource == null) {
   println ">> ERROR: configSource variable for ${buildFile} is not set"
   return -1
}

// Check if configuration file exists
File configFile = new File(configSource)
if (!configFile.exists()) {
   println ">> WARNING: File ${configSource} does not exist."
   return 0
}

// parse YAML configuration file for build file
println ">> Setting overrides from ${configSource}"
def yaml = new YamlSlurper().parse(configFile)

// loop though each YAML variable and set the language variable
// note that we set it as a file variable for the build file
yaml.config.each{ variable ->
   config.setFileVariable(variable.name, variable.value, buildFile)
}

return 0