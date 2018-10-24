//*****************************************************************************************
//* usedel:                                                                               *
//*         Remove a user from the system                                                 *
//*                                                                                       *
//* Dependencies:                                                                         *
//*              apache commons cli (https://commons.apache.org/proper/commons-cli/)      *
//*              this not only handles the arguments for the add, it does the documention *
//*                                                                                       *
//*****************************************************************************************
import com.ibm.dbb.build.*
import org.apache.commons.cli.*

// Take input
def cli= new CliBuilder(usage: 'userdel [options] userid ')
cli.h(longOpt: 'help', 'Usage Information', required: false)
cli.t(longOpt: 'trace','Trace activity', required:false)
cli.v(longOpt: 'verbose', 'Print out intermediate steps', required:false)

def options=cli.parse(args)
userid=options.arguments()[0]

// Do I need to trace and print stuff out
if (options.t) trace=true
else trace=false
if (options.v) verbose=true
else verbose=false

if (verbose) println("userid: ${userid} trace: ${trace} verbose: ${verbose}")
println("Deleting user: ${userid}")

//First create a TSO command object
TSOExec cmd= new TSOExec();

//set the Configuration directory (we have hardcoded this)
cmd.setConfDir("/var/dbb/conf");

//define the DD statment needed for the EXEC
// the HLQ should be changed to your high level qualified
cmd.dd(new DDStatement().name("CMDSCP").dsn("HLQ.ISPFGWY.EXEC").options("shr"))

//First we need to delete the dataset profile
if (verbose) println("Deleting the dataset profile")

deldSDCommand="DELDSD  ('${userid}.**')"
if (verbose) println ("Command: ${delSDCommand}")

cmd.setCommand(deldSDCommand)
File dsdlog= new File("./delsd.log")
cmd.setLogFile(dsdlog)
rc=cmd.execute()
if (rc!=0){
   println("DELDSD Command Failed! Return Code: ${rc}")
   if (!trace) dsdlog.delete()
   System.exit(rc);
 } else dsdlog.delete()

 //Next delete the user from RACF
 deleteUserCommand="DELUSER (${userid})"

 if (verbose) println("Deleting the user to RACF")
 if (verbose) println("Command: ${deleteUserCommand}")
 cmd.setCommand(deleteUserCommand);
 File dellog= new File("./delUser.log")
 cmd.setLogFile(dellog)
 rc=cmd.execute()
 if (rc!=0){
   println("delUser Command Failed! Return Code: ${rc}")
   if (!trace) dellog.delete()
   System.exit(rc);
 } else dellog.delete()


// Finally delete the alias...
aliasCommand="DEL ('${userid}') ALIAS"
if (verbose) println("Deleting Alias")
if (verbose) println("Command: ${aliasCommand}")
cmd.setCommand(aliasCommand)
File aliaslog= new File("./Alias.log");
cmd.setLogFile(aliaslog)
rc=cmd.execute()
if (rc!=0){
    println("Alias Command Failed! Return Code: ${rc}")
	if (!trace) aliaslog.delete()
    System.exit(rc)
} else aliaslog.delete()
   println("User deleted!")
