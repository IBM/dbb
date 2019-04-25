import com.ibm.dbb.build.*
import com.ibm.dbb.build.DBBConstants.CopyMode;

// create PDS
def srcOptions = "cyl space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library) msg(1)"
new CreatePDS().dataset("USR.INDIV").options(srcOptions).create()
new CreatePDS().dataset("USR.ALL").options(srcOptions).create()

// copy individual binary file with custom member name
cmd = new CopyToPDS().dataset("USR.INDIV").member("CUSTOM").archive("archive/archive_bin.tar").archivedFile("bin/EPSMLIST").copyMode(CopyMode.BINARY)
cmd.execute()

// copy individual binary file with standard member name
cmd = new CopyToPDS().dataset("USR.INDIV").archive("archive/archive_bin.tar").archivedFile("bin/EPSMLIST").copyMode(CopyMode.BINARY)
cmd.execute()

// copy all binary files
cmd = new CopyToPDS().dataset("USR.ALL").archive("archive/archive_bin.tar").copyMode(CopyMode.BINARY)
cmd.execute()

// copy individual text file with custom member name
cmd = new CopyToPDS().dataset("USR.INDIV").member("TXT").archive("archive/archive_src.tar").archivedFile("src/HELLO.txt")
cmd.execute()

// copy individual text file with custom member name from compressed file
cmd = new CopyToPDS().dataset("USR.INDIV").member("TXTGZ").archive("archive/archive_src.tar.gz").archivedFile("src/HELLO.txt")
cmd.execute()

// copy individual text file with standard member name
cmd = new CopyToPDS().dataset("USR.INDIV").archive("archive/archive_src.tar").archivedFile("src/HELLO.txt")
cmd.execute()

// copy all textmode files
cmd = new CopyToPDS().dataset("USR.ALL").archive("archive/archive_src.tar")
cmd.execute()