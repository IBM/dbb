import com.ibm.dbb.build.*
import com.ibm.dbb.build.DBBConstants.CopyMode;

def delete = new MVSExec().pgm("IEFBR14")
delete.dd(new DDStatement().name("OLDDS").dsn("USR.INDIV").options("old delete"))
def rc = delete.execute()

delete = new MVSExec().pgm("IEFBR14")
delete.dd(new DDStatement().name("OLDDS").dsn("USR.ALL").options("old delete"))
rc = delete.execute()