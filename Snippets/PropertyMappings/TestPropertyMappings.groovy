@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*


def mapping = new PropertyMappings("theNewName")
//load the properties
def properties = BuildProperties.getInstance()
def theFile = new File("${getScriptDir()}/noscriptmapping.properties")

println(" Print values of the key : ${mapping.getKeyName()}")

//load a property file with the key theNewName 
properties.load(theFile)
//Print the values of this file
mapping.getValues().each{value->
	println(value)
}

//Set the key scriptMapping
println("\nSet the key as anotherName")
mapping.setKeyName("anotherName")
println(" Print values of the keyword : ${mapping.getKeyName()}")
//Print the values of file.properties
mapping.getValues().each{value->
	println(value)
}

//add one
def setlist = [] as List<String>
def another = "Three"
def filepat = "MortgageApplication/cobol/*.cbl"
def value2 = "Five"

setlist.add("MortgageApplication/mfs_2/*.mfs")
println("\n Try setValue -> ${another} : ${setlist[0]}")
mapping.setValue(another,setlist)
println(" Try addFilePattern -> ${value2} : ${filepat} \n")
mapping.addFilePattern(value2,filepat)

println(" Print again values of the key : ${mapping.getKeyName()}")
mapping.getValues().each{value->
	println(value)
}
///
	//Print all the properties.
	println("\n Print all the properties :")
	println(properties.list())
	
	//Replace the file of the four
	println("\nReplace Four")
	def setlist2 = [] as List<String>
	setlist2.add("C:/Users/JeromeSarrasin/git/MortgageApplication/cobol_cics/*.cbl")
	mapping.setValue("Four",setlist2)
	
	//Print all the properties.
	println("\n Print all the properties :")
	println(properties.list())
	
	//Add a file for the value Four
	println("\nAdd a file for the value Four")
	mapping.addFilePattern("Four","C:/Users/JeromeSarrasin/git/MortgageApplication/copybook/*.cpy")
	
	//Print all the properties.
	println("\n Print all the properties :")
	println(properties.list())
