@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript

/*
* This step task Groovy script creates a ${FILE_NAME_BASE} variable
* that contains the file name without the extension.  
* Ex: MortgageApplication/cobol/epsnbrvl.cbl --> epsnbrvl
*/

// get language variable from pre-defined language config object
String fileName = config.getVariable("FILE_NAME")

// strip off the file extension
int idx = fileName.lastIndexOf('.')
if (idx > -1)
  fileNameBase = fileName.substring(0, idx);

// if this is a hidden (dot) file then reset to FILE_NAME
if (fileNameBase.length() == 0)
  fileNameBase =  fileName

// set the new variable
config.setVariable("FILE_NAME_BASE", fileNameBase)
println "> Creating variable FILE_NAME_BASE = ${fileNameBase}"

return 0