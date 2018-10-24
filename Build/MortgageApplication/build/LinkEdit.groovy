@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*

// receive passed arguments
def file = argMap.file
println("* Building $file using ${this.class.getName()}.groovy script")

// define local properties
def properties = BuildProperties.getInstance()
def linkPDS = "${properties.hlq}.LINK"
def objectPDS = "${properties.hlq}.OBJ"
def loadPDS = "${properties.hlq}.LOAD"
def member = CopyToPDS.createMemberName(file)
def logFile = new File("${properties.workDir}/${member}.log")

// create a reference to the Tools.groovy utility script
def tools = loadScript(new File("Tools.groovy"))

// define the BPXWDYN options for allocated temporary datasets
def tempCreateOptions = "tracks space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"

// copy program to PDS 
println("Copying ${properties.sourceDir}/$file to $linkPDS($member)")
new CopyToPDS().file(new File("${properties.sourceDir}/$file")).dataset(linkPDS).member(member).execute()

// Link-edit the build file
println("Link editing link file $file")	

// define the MVSExec command to link edit the program
def linkedit = new MVSExec().file(file).pgm("IEWBLINK").parm("MAP,RENT,COMPAT(PM5)")
	                    
// add DD statements to the linkedit command
linkedit.dd(new DDStatement().name("SYSLIN").dsn("$linkPDS($member)").options("shr").report(true))
linkedit.dd(new DDStatement().name("SYSLMOD").dsn("$loadPDS($member)").options("shr").output(true).deployType("LOAD"))
linkedit.dd(new DDStatement().name("SYSPRINT").options(tempCreateOptions))
linkedit.dd(new DDStatement().name("SYSUT1").options(tempCreateOptions))
linkedit.dd(new DDStatement().name("SYSLIB").dsn(objectPDS).options("shr"))
linkedit.dd(new DDStatement().dsn(properties.SCEELKED).options("shr"))
linkedit.dd(new DDStatement().dsn(properties.SDFHLOAD).options("shr"))

// add a copy command to the linkedit command to append the SYSPRINT from the temporary dataset to the HFS log file
linkedit.copy(new CopyToHFS().ddName("SYSPRINT").file(logFile).hfsEncoding(properties.logEncoding))

// execute the link edit command
def rc = linkedit.execute()

// update build result
tools.updateBuildResult(file:"$file", rc:rc, maxRC:0, log:logFile)

// Scan the load module to determine LINK dependencies. Impact resolver can use these to determine that
// this file gets rebuilt if a LINK dependency changes.
if ( rc == 0 && !properties.userBuild ) {
	println("* Scanning $loadPDS($member) for load module dependencies.")
	def scanner = new LinkEditScanner()
   	def logicalFile = scanner.scan(file, loadPDS)

	// create the outputs collection if needed.
	// NOTE: The outputs collection should be separate from properties.collection otherwise these dependencies will
	//       be overwritten when the source is changed and scanned by source code scanner.
	// NOTE: The outputs collection must be included in ImpactResolver in Tools.groovy to include these outputs
	//       during impact analysis.
	def outputs_collection = "${properties.collection}_outputs"
	def repositoryClient = tools.getDefaultRepositoryClient()
	if (!repositoryClient.collectionExists(outputs_collection)) {
    	repositoryClient.createCollection(outputs_collection) 
	}
	
	// Store logical file and indirect dependencies to the outputs collection
	repositoryClient.saveLogicalFile( outputs_collection, logicalFile );
}
