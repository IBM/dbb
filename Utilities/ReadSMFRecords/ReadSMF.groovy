@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.build.*
import com.ibm.dbb.build.smf.*

import com.ibm.jzos.ZFile
import com.ibm.jzos.ZFileConstants
import com.ibm.jzos.ZFileException
import com.ibm.jzos.RecordReader

// load the tools.groovy utility script
def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
def tools = loadScript(new File("$scriptDir/tools.groovy"))

// parse command line arguments and load build properties
def opts = tools.parseArgs(args)
def properties = tools.loadProperties(opts)
tools.validateRequiredProperties(["hlq", "datasets"])

def startTime = new Date()
properties.startTime = startTime.format("yyyyMMdd.hhmmss.mmm")
println("** Read SMF start at $properties.startTime")

// Set name or working dataset...After running the utility, this dataset contains the raw SMF records you've requested.
def smfwrk="${properties.hlq}.SMF.WRK"
try
{
    ZFile.remove("//'${smfwrk}'");
}
catch (ZFileException ex)
{}

// Define options for various datasets we will use
def sysprintOptions = "tracks space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new"
def smfoutOptions = "CYL space(800,200) unit(SYSDA) new keep"
def smfwrkOptions = "new keep CYL space(100,10) lrecl(32756) dsorg(PS) recfm(V,B) BLKSIZE(32760)"

// Log files for the Dump Utility(IFASMFDP) and IDCAMS
def smflogFile = new File("${properties.workDir}/smfDump.log")
def reprologFile = new File("${properties.workDir}/repro.log")

// Commands for dump utility.
// For each dataset defined in SYS1.PARMLIB(SMFPRMxx) DSNAME parameter, add a INDD statement to commands.
// Also add a single OUTDD statement to indicate what SMF records types to dump	
def smfDumpParms = ""
def i = 0
properties.datasets.split(',').each{ dataset ->
	smfDumpParms += "INDD(SMFIN${++i},OPTIONS(DUMP))\n"
}
// By default, gets types 0-255. Use -i option to specify specific type
smfDumpParms += """OUTDD(SMFOUT,TYPE(${(properties.recordType == null)?"0:255":properties.recordType}))"""

// Set up SMF Dump utility to dump the SMF records to a temporary dataset...SMFOUT
def smfDump = new MVSExec().pgm("IFASMFDP")
i = 0
properties.datasets.split(',').each{ dataset ->
	smfDump.dd(new DDStatement().name("SMFIN${++i}").dsn(dataset).options("shr"))
}
smfDump.dd(new DDStatement().name("SMFOUT").dsn("&&SMFOUT").options(smfoutOptions).pass(true))
smfDump.dd(new DDStatement().name("SYSIN").instreamData(smfDumpParms))
smfDump.dd(new DDStatement().name("SYSPRINT").options(sysprintOptions))
smfDump.copy(new CopyToHFS().ddName("SYSPRINT").file(smflogFile).hfsEncoding(properties.logEncoding))

// Set up IDCAMS to copy SMF records from the temporary data to one we can use and read
def repro = new MVSExec().pgm("IDCAMS")
repro.dd(new DDStatement().name("SMFWRK").dsn(smfwrk).options(smfwrkOptions))
repro.dd(new DDStatement().name("SYSIN").instreamData(" REPRO INFILE(SMFOUT) OUTFILE(SMFWRK)"))
repro.dd(new DDStatement().name("SYSPRINT").options(sysprintOptions))
repro.copy(new CopyToHFS().ddName("SYSPRINT").file(reprologFile).hfsEncoding(properties.logEncoding))

// Run the Dump and IDCAMS commands
def rc = new MVSJob().executable(smfDump).executable(repro).maxRC(4).execute()
println("IFASMFDP/IDCAMS rc=$rc")

StringBuffer report = new StringBuffer()
tools.startReport(report)
// Now data is in ${smfwrk}. We will read this data directly from dataset so we can read record by record 
if ( rc == 0 && ZFile.dsExists("//'${smfwrk}'") )
{
	tools.startReportTable(report)
	RecordReader reader = null;
	def count = 0
	try
	{
		// Read each record from the working dataset
		reader = RecordReader.newReader("//'${smfwrk}'", ZFileConstants.FLAG_DISP_SHR);
		byte[] buffer = new byte[reader.getLrecl()];
		while ((bytesRead = reader.read(buffer)) >= 0)
		{
			byte[] data = Arrays.copyOf(buffer, bytesRead)
			def smfRec = new SmfRecord(data, true)
			if ( !tools.isSmfTypeInteresting(smfRec.getType()) )
				continue
			// Add SMF information to the html table
			tools.reportSMFRecord( report, data )
			count++
		}
	}
	finally
	{
		if (reader != null)
		{
			reader.close(); 
		}
	}
	tools.endReportTable(report)
	report.append("""
Total records: $count
""")
}
tools.endReport(report)
tools.writeReport(report)
