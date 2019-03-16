//*****************************************************************************************
//* useradd:                                                                              *
//*         Add a user to the system                                                      *
//*                                                                                       *
//* Dependencies:                                                                         *
//*              apache commons cli (https://commons.apache.org/proper/commons-cli/)      *
//*              this not only handles the arguments for the add, it does the documention *
//*                                                                                       *
//* Additional Components:                                                                *
//*              useradd.properties - a file that holds the defaults for adding a user    *
//*                                   this allows an installtion to create a set of       *
//*                                   defaults for different user types.                  *
//*****************************************************************************************
import com.ibm.dbb.build.*
import org.apache.commons.cli.*

// Take input and define any overrides to the defaults
def cli= new CliBuilder(usage: 'useradd -u userid -n "users name"')
cli.h(longOpt: 'help', 'Usage Information', required: false)
cli.p(longOpt: 'properties' , 'proprties file', args:1 , required: false )
cli.u(longOpt: 'user', 'userid', args:1 , required: true )
cli.n(longOpt: 'name','users name (surrounded by double quotes)', required: true , args:1)
cli._(longOpt:'usercat' , 'user catalog' , required:false , args:1)
cli._(longOpt: 'mstrcat', 'master catalog' , required:false , args:1)
cli._(longOpt:'password', 'default password', required:false , args:1)
cli._(longOpt:'owner', 'owner', required:false , args:1)
cli._(longOpt:'dfltgrp', 'default group', required:false , args:1)
cli._(longOpt:'uacc', 'RACF UACC' , required:false , args:1)
cli._(longOpt:'auth', 'Authority' , required:false , args:1)
cli._(longOpt:'data' , 'User Data' , required:false , args:1)
cli._(longOpt:'prgm', 'Shell Program', required:false , args:1)
cli._(longOpt:'acctnum' , 'Accound Number' , required:false , args:1 )
cli._(longOpt:'jobclass' , 'Job Class' , required:false , args:1)
cli._(longOpt:'msgclass' , 'Message Class', required:false , args:1)
cli._(longOpt:'proc' , 'TSO PROC' , required:false , args:1)
cli._(longOpt:'size' , 'Region size' , required:false , args:1)
cli._(longOpt:'sysoutcl', 'Sysout Class' , required:false , args:1)
cli._(longOpt:'unit' , 'Unit', required:false , args:1)
cli._(longOpt: 'userdat' , 'User data' , required:false ,  args:1)
cli._(longOpt:'sduacc', 'Data set profile uacc' , required:false , args:1)
cli.t(longOpt: 'trace', 'Trace activity', required:false)
cli.v(longOpt: 'verbose' , 'Verbose printing' , required:false)


def options=cli.parse(args)

if (!options) return
if (options.h) cli.usage()

// Have a base set of defaults so someone doing the useradd might not need to
// to know that there are a set of paramters needed to add users
if (!options.p) propFile="/etc/IBM/useradd.properties"
else propFile=options.p

// Define the userid and the name of the person the id is for
userid=options.u
name=options.n

// Do I need to trace and print stuff out
if (options.t) trace=true
else trace=false
if (options.v) verbose=true
else verbose=false

if (verbose) println("Using properties file: ${propFile}")

// Get properties we will use for defining the users
def properties=BuildProperties.getInstance()
properties.load(new File(propFile))
validate(propFile)

// See if any of the properties in the properties file are overridden
if (options.usercat) properties.usercat=options.usercat
if (options.password) properties.dfltpassword=options.password
if (options.owner) properties.owner=options.owner
if (options.dfltgrp) properties.dfltgrp=options.dfltgrp
if (options.uacc) properties.uacc=options.uacc
if (options.auth) properties.authority=options.auth
if (options.data) properties.data=options.data
if (options.prgm) properties.program=options.prgm
if (options.acctnum) properties.acctnum=options.actnum
if (options.jobclass) properties.jobclass=options.jobclass
if (options.msgclass) properties.msgclass=options.msgclass
if (options.proc) properties.proc=options.proc
if (options.unit) properties.unit=options.unit
if (options.userdat) properties.userdata=options.userdat
if (options.sduacc) properties.sduacc=options.sduacc

println("Adding user: ${userid}")
if (verbose){
	println("properties.usercat: ${properties.usercat}")
	println("properties.mastercat: ${properties.mastercat}")
	println("properties.dfltpassword: ${properties.dfltpassword}")
	println("properties.owner: ${properties.owner}")
	println("properties.dfltgrp: ${properties.dfltgrp}")
	println("properties.uacc: ${properties.uacc}")
	println("properties.authority: ${properties.authority}")
	println("properties.data: ${properties.data}")
	println("properties.program: ${properties.program}")
	println("properties.acctnum: ${properties.acctnum}")
	println("properties.jobclass: ${properties.jobclass}")
	println("properties.msgclass: ${properties.msgclass}")
	println("properties.proc: ${properties.proc}")
	println("properties.unit: ${properties.unit}")
	println("properties.userdata: ${properties.userdata}")
	println("properties.sduacc: ${properties.sduacc}")
}

//lets create a TSO command object
TSOExec cmd= new TSOExec();

//set the Configuration directory (we have hard coded this)
cmd.setConfDir("/var/dbb/conf");

//define the DD statment needed for the EXEC (We have hard coded this)
//HLQ should be modified to be a base ID that you need.
cmd.dd(new DDStatement().name("CMDSCP").dsn("HLQ.ISPFGWY.EXEC").options("shr"))

// First lets define an alias...
aliasCommand="DEFINE ALIAS (NAME('${userid}') RELATE('${properties.usercat}')) CATALOG('${properties.mastercat}')"
if (verbose) println("Creating Alias")
if (verbose) println("Command: ${aliasCommand}")
cmd.setCommand(aliasCommand)
File aliaslog= new File("./Alias.log")
cmd.setLogFile(aliaslog)
rc=cmd.execute()
if (rc!=0){
	println("Alias Command Failed! Return Code: ${rc}")
	if (!trace) aliaslog.delete()
    System.exit(rc)
} else {
    aliaslog.delete()
}

//Lets build the RACF adduser command
addUserCommand="addUser (${userid}) password(${properties.dfltpassword}) name('${name}')"
addUserCommand="${addUserCommand} OWNER(${properties.owner}) DFLTGRP(${properties.dfltgrp}) UACC(${properties.uacc})"
addUserCommand="${addUserCommand} AUTHORITY(${properties.authority}) DATA('${properties.data}')"
addUserCommand="${addUserCommand} OMVS(AUTOUID HOME('/u/${userid}') PROGRAM('${properties.program}'))"
addUserCommand="${addUserCommand} TSO(ACCTNUM(${properties.acctnum}) JOBCLASS(${properties.jobclass}) MSGCLASS(${properties.msgclass}) PROC(${properties.proc})"
addUserCommand="${addUserCommand} SIZE(${properties.size}) SYSOUTCLASS(${properties.sysoutclass}) UNIT(${properties.unit}) USERDATA(${properties.userdata}))"
if (verbose) println("Adding the user to RACF")
if (verbose) println("command: ${addUserCommand}")
cmd.setCommand(addUserCommand)
File addlog= new File("./addUser.log")
cmd.setLogFile(addlog)
rc=cmd.execute()
if (rc!=0){
	println("addUser Command Failed! Return Code: ${rc}")
	if (!trace) addlog.delete()
    System.exit(rc)
} else {
	addlog.delete()
}

if (verbose) println("Creating the dataset profile")

//Now we need a dataset profile
addSDCommand="ADDSD  ('${userid}.**') UACC(${properties.sduacc}) OWNER(${userid}) GENERIC "
if (verbose) println ("command: ${addSDCommand}")
cmd.setCommand(addSDCommand)

File asdlog= new File("./addsd.log")
cmd.setLogFile(asdlog)
rc=cmd.execute()
if (rc!=0){
   println("ADDSD Command Failed! Return Code: ${rc}");
   if (!trace) asdlog.delete()
   System.exit(rc);
} else {
   asdlog.delete()
}
println("User created!")

// Make sure that the right entries are in the file.
def validate(String propFile){
  def properties = BuildProperties.getInstance()
  assert properties.dfltpassword : "Missing property 'dfltpassword'. Please edit ${propFile}"
  assert properties.usercat: "Missing property 'usercat'. Please edit ${propFile}"
  assert properties.mastercat: "Missing property 'mastercat'. Please edit ${propFile}"
  assert properties.owner : "Missing property 'owner'. Please edit ${propFile}"
  assert properties.dfltgrp : "Missing property 'dfltgrp'. Please edit ${propFile}"
  assert properties.uacc : "Missing property 'uacc'. Please edit ${propFile}"
  assert properties.authority : "Missing property 'authority'. Please edit ${propFile}"
  assert properties.data : "Missing property 'data'. Please edit ${propFile}"
  assert properties.program : "Missing property 'program'. Please edit ${propFile}"
  assert properties.acctnum : "Missing property 'acctnum'. Please edit ${propFile}"
  assert properties.jobclass : "Missing property 'jobclass'. Please edit ${propFile}"
  assert properties.msgclass : "Missing property 'msgclass'. Please edit ${propFile}"
  assert properties.proc : "Missing property 'proc'. Please edit ${propFile}"
  assert properties.size : "Missing property 'size'. Please edit ${propFile}"
  assert properties.sysoutclass : "Missing property 'sysoutclass'. Please edit ${propFile}"
  assert properties.unit : "Missing property 'unit'. Please edit ${propFile}"
  assert properties.userdata : "Missing property 'userdata'. Please edit ${propFile}"
  assert properties.sduacc : "Missing property 'sduacc'. Please edit ${propFile}"

}
