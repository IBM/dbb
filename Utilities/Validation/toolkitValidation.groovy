@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.build.*

//Get DBB toolkit product info
version = new VersionInfo()
println(" ")
println("Info about this DBB installation")
println("--------------------------------")
println("DBB Version: " + version.getVersion())
println("Build Number: " + version.getBuild())
println("Release Date: " + version.getDate())
println(" ")

// execute IEFBR14
def doAlmostNothing = new MVSExec().pgm("IEFBR14")
def rc = doAlmostNothing.execute()

println("Test MVSExec of IEFBR14")
println("-----------------------")
println("RC: " + rc)
println(" ")
