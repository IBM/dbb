import com.ibm.dbb.build.*

//accept suffix as passed agrument from Jenkins
def suffix = args[0]
println('arg:' + suffix)

//call copy function passing suffix 
copyDatasets(suffix)

def copyDatasets(String suffix) {
	//define patterns to search for file extentions and copy them into the correct libraries on zos
	def rexpattern = '.rex'
	def skelpattern = '.skel'
	def pnlpattern = '.pnl'
	def pmsgpattern = '.pmsg'
	def cntlpattern = '.cntl'
	def jclpattern = 'jcl'
	def rexPDS = suffix +'.ISPCLIB'
	def skelPDS = suffix +'.ISPSLIB'
	def pnlPDS = suffix +'.ISPPLIB'
	def pmsgPDS = suffix +'.ISPMLIB'
	def cntlPDS = suffix +'.CNTL'
	def jclPDS = suffix +'.JCL'
	def processCounter = 0

	// create Rexx list from input build file
	def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
	
	//use the files.txt in the git repo to drive what files are copied.
	def copyList = new File("$scriptDir/files.txt").readLines()
	println('CopyList:' + copyList)

	if (copyList.size() == 0)
	  println("** No files in files.txt.  Nothing to copy.")
	else {
	  copyList.each { list ->
	    // search for pattern and copy file to correct library
	    if (list.find(rexpattern)) {
	      println("Copying $list to $rexPDS")
	      new CopyToPDS().file(new File(list)).dataset(rexPDS).execute()  
	    }
	    if (list.find(skelpattern)) {
	      println("Copying $list to $skelPDS")
	      new CopyToPDS().file(new File(list)).dataset(skelPDS).execute()
		}
		if (list.find(pnlpattern)) {
		  println("Copying $list to $pnlPDS") 
		  new CopyToPDS().file(new File(list)).dataset(pnlPDS).execute()
		}
		if (list.find(pmsgpattern)) {
		  println("Copying $list to $pmsgPDS")
		  new CopyToPDS().file(new File(list)).dataset(pmsgPDS).execute()
		}
		if (list.find(cntlpattern)) {
		  println("Copying $list to $cntlPDS")
		  new CopyToPDS().file(new File(list)).dataset(cntlPDS).execute()
		}
		if (list.find(jclpattern)) {
		  println("Copying $list to $jclPDS")
		  new CopyToPDS().file(new File(list)).dataset(jclPDS).execute()
		}
		processCounter++
		}
	}
}
