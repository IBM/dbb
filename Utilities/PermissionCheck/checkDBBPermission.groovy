import groovy.transform.*
import com.ibm.os390.security.PlatformThread
import com.ibm.os390.security.PlatformUser
import groovy.cli.commons.*

def properties = parseInput(args)

println("-- DBB Role Checker --")
println()

if (properties.userId) {
	println("-- Checking UserId provided via CLI")
	checkRole(properties.userId)
}

println("-- Checking Current User")
def platformUser = PlatformThread.getUserName()
checkRole(platformUser)


def parseInput(String[] cliArgs){
	def scriptProperties = new Properties()
	
	def cli = new CliBuilder(usage: "checkDBBRole.groovy [options]")
	cli.u(longOpt:'userId', args:1, argName:'userId', 'Name of TSO User to check for the DBB role.')
	
	def opts = cli.parse(cliArgs)
	
	if (opts.u) scriptProperties.userId = opts.u
	
	return scriptProperties
}

def checkRole(String userId){
	println("- Check DBB Role for user $userId:")
	println("- Member of SAF Group")
	println(" DBBADMNS : " + PlatformUser.isUserInGroup( userId, "DBBADMNS" ))
	println(" DBBUSERS : " + PlatformUser.isUserInGroup( userId, "DBBUSERS" ))
	println(" DBBGUEST : " + PlatformUser.isUserInGroup( userId, "DBBGUEST" ))
	println()
}