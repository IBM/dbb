import com.ibm.jzos.ZFile
import com.ibm.dbb.build.CopyToPDS

if (args.length < 2) {
    println("Usage: uploadPDSMember.groovy PDS_NAME FILE1 FILE2...")
    System.exit(1);
}

String DSN = args[0]

if (ZFile.dsExists("//'" + DSN + "'") == false) {
    println("The PDS '$DSN' does not exist. Exiting.")
    System.exit(1)
}

for (int i=1; i<args.length; i++) {
    File file = new File(args[i])
    if (file.isFile() == false) continue

    println("Uploading: ${args[i]}")
    int rc = new CopyToPDS().dataset(DSN).file(file).execute()
    if (rc != 0) {
        println("Upload Failure")
        System.exit(rc)
    }
}
