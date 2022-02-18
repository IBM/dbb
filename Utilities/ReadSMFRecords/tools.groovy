import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.nio.ByteBuffer

import com.ibm.dbb.build.*
import com.ibm.dbb.build.internal.Messages
import com.ibm.dbb.build.smf.*

import groovy.cli.commons.*

def parseArgs(String[] cliArgs){
	def cli = new CliBuilder(usage: "readSMF.groovy [options]",header: "\nAvailable options\n")
	cli.h(longOpt: 'help','help')
	cli.q(longOpt: 'hlq',args:1,optionalArg:true,argName:'hlq','High level qualifier for working dataset.')
	cli.t(longOpt: 'type', args:1,optionalArg:true, argName:'type','SMF type to process. Can be a number or range (i.e. 0:255)')
	cli.d(longOpt: 'datasets', args:1, valueSeparator:',', optionalArg:true, argName:'datasets','Comma separated list of SMF datasets as defined in SYS1.PARMLIB(SMFPRMxx) DSNAME option')
	cli.w(longOpt: 'workDir', args:1, argName:'dir', 'Absolute path to the build output directory')
	cli.e(longOpt: 'logEncoding', args:1, argName:'encoding', 'Encoding of output logs. Default is EBCDIC')
	
	
	def opts = cli.parse(cliArgs)
	if(opts.h)
	{
		cli.usage()
		System.exit(0)
	}
	
	if(opts.q && opts.q instanceof Boolean)
	{
		throw new BuildException("-q needs you to specify a high level qualifier. Run with -h to view details.")
	}
	
	return opts
}


def loadProperties(OptionAccessor opts) {
	// check to see if there is a ./build.properties to load
	def properties = BuildProperties.getInstance()
	def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
	def buildPropFile = new File("$scriptDir/smf.properties")
	if (buildPropFile.exists())
		   BuildProperties.load(buildPropFile)

	// set command line arguments
	if (opts.q) properties.hlq = opts.q
	if (opts.t)
	{	
		if (opts.t instanceof Boolean)
			properties.recordType = "122"
		else
			properties.recordType = opts.t
	}
	if (opts.d) properties.datasets = opts.d
	if (opts.w) properties.workDir = opts.w
	if (opts.e) properties.logEncoding = opts.e
	
	validateType(properties.recordType)
	
	println("Datasets: ${properties.datasets}")
	println("** Build properties at startup:")
	println(properties.list())

	return properties
}

def validateRequiredProperties(List<String> props) {
	def properties = BuildProperties.getInstance()
	props.each { prop ->
		assert properties."$prop" : "Missing property $prop"
	}
}

def validateType( String type )
{
	// if type is null...okay
	if ( !type )
		return true
		
	// if type is a number...also okay
	if ( type.isNumber() )
		return true

	// validate that all characters are acceptable
	if (!type.matches("[0-9,:\\s]+"))
	{	
			throw new BuildException("The record type parameter, ${type}, contains an invalid character. Valid characters are 0-9, comma(,), and colon(:).")
	}
	
	// check each comma separated segment for a number or number:number
	def segments = type.split(',') // treat as comma separated list
	segments.each{ segment ->
		if (!segment.isNumber() && !(segment.contains(':') && segment.split(':')[0].isNumber() && segment.split(':')[1].isNumber()))
		{
			throw new BuildException("The record type, ${segment}, is not a valid SMF record type. Please enter a single number or two numbers separated by a colon(:)")
		}
	}
}

// Determine if type is among the SMF types that were requested
def isSmfTypeInteresting( int type )
{
	// never interested in type 2 or 3. These only exist in dump dataset
	if (type == 2 || type == 3 )
		return false;
		
	def properties = BuildProperties.getInstance()
	
	// If no record type specified, accept all records
	if (properties.recordType == null )
		return true

	// If valid number...check to see if type is that number
	if (properties.recordType.isNumber())
		return (properties.recordType.toInteger() == type)

	// check each comma separated segment for a number or number:number
	def segments = properties.recordType.split(',') // treat as comma separated list
	for ( String segment: segments )
	{
		// If segment is a valid number...check to see if type is that number
		if (segment.isNumber() && segment.toInteger() == type)
			return true
			
		// If int:int type input, check range...inclusive
		if (segment.contains(':') && segment.split(':')[0].toInteger() <= type && segment.split(':')[1].toInteger() >= type)
			return true
	}
	
	return false	
}

def byteToHex(byte b)
{
   byte[] a = new byte[1];
   a[0] = b;
   return "0x"+a.encodeHex().toString().toUpperCase()
}

def bytesToHex(byte[] bytes, int offset, int length)
{
   byte[] newBytes = new byte[length];
   int i=0,j=offset
   while ( i < length )
   {
	  newBytes[i] = bytes[j]
	  i++
	  j++
   }
   return "0x"+newBytes.encodeHex().toString().toUpperCase()
}

def bytesToInt(byte[] bytes, int offset)
{
   ByteBuffer buffer = ByteBuffer.wrap(bytes, offset, 4)
   return buffer.getInt()
}

def bytesToShort(byte[] bytes, int offset)
{
   ByteBuffer buffer = ByteBuffer.wrap(bytes, offset, 2)
   return buffer.getShort()
}

def bytesToString(byte[] bytes, int offset, int length)
{
   byte[] newBytes = new byte[length];
   int i=0,j=offset
   while ( i < length )
   {
	  newBytes[i] = bytes[j]
	  i++
	  j++
   }
   return new String(newBytes, Utils.retrieveEncoding())
}

// Writes the report HTML out to a file in the working directory
def writeReport(StringBuffer content) throws BuildException
{
	def properties = BuildProperties.getInstance()
	
	def reportEncoding = "UTF-8"
	def file = new File("${properties.workDir}/SMFReport.html")
	println("writing report to $file")
	FileOutputStream fos = null
	try
	{
		file.getParentFile().mkdirs()
		fos = new FileOutputStream(file)
		fos.write(content.toString().getBytes(reportEncoding))
		fos.flush()
	}
	catch (IOException e)
	{
		throw new BuildException(Messages.getMessage("Error_CREATING_FILE", file.getAbsoluteFile(), e))
	}
	finally
	{
		if (fos != null)
			fos.close()
	}
}

// Adds HTML header, stype and body tags to report
def startReport( StringBuffer report)
{
	report.append("""<!DOCTYPE html>
<html>

<head>
	<style type="text/css">
		body {
    		font: 12px Arial, sans-serif;
		}

		table {
    		border-collapse: collapse;
		}

		table, th, td {
    		border: 1px solid #A9A9A9;
		}

		th, td {
    		padding: 10px;
    		text-align: left;
		}

		tr.even {
			background-color: #f2f2f2
		}

		th {
    		background-color: #81C4E8;
    		color: white;
		}

		td.label {
			font-weight: bold;
		}

		.bottomRight {
			float:right;
			text-align:right;
			font: 10px Arial, sans-serif;
		}

		.dep {
			font: 11px Arial, sans-serif;
		}

		.depType {
			font: 9px Arial, sans-serif;
			background-color: #81C4E8;
		}
	</style>
</head>

<body>
	<h2>SMF Report</h2>
	<p id="main"></p>""")
}

// Completes the report HTML
def endReport( StringBuffer report)
{
	report.append("""
</body>
</html>""")
}

// Start the Report table with headers
def startReportTable( StringBuffer report)
{
	report.append("""
	<table border="1" cellpadding="4" summary="SMF fields">
		<tr>
			<th style="text-align:left">Flags</th>
			<th style="text-align:left">Type</th>
			<th style="text-align:left">Time</th>
			<th style="text-align:left">Date</th>
			<th style="text-align:left">SID</th>
			<th style="text-align:left">SSI</th>
			<th style="text-align:left">Sub-Type</th>
			<th style="text-align:left">Product Name</th>
			<th style="text-align:left">Product Feature</th>
			<th style="text-align:left">Product ID</th>
			<th style="text-align:left">Userid</th>
			<th style="text-align:left">Version</th>
			<th style="text-align:left">Release</th>
			<th style="text-align:left">Modification</th>
			<th style="text-align:left">Build</th>
			<th style="text-align:left">Timestamp</th>
		</tr>""")
}

// Completes the report table
def endReportTable( StringBuffer report)
{
	report.append("""
	</table>""")
}

// Add an SMF record to the report table
def reportSMFRecord( StringBuffer report, byte[] data )
{
	def smfRec = new SmfRecord(data, true)
	// Report the header portion of SMF record
	report.append("""
		<tr>
			<td>${byteToHex(smfRec.getFlag())}</td>
			<td>${smfRec.getType()}</td>
			<td>${bytesToInt(smfRec.getTime(),0)}</td>
			<td>${bytesToHex(smfRec.getDate(),0,4)}</td>
			<td>${smfRec.getSID()}</td>""")
	// If flag set, this is a header for SMF record with subtype
	if ( smfRec.getFlag() & 0x40 )
	{
		report.append("""
			<td>${smfRec.getWID()}</td>
			<td>${smfRec.getSubType()}</td>""")
	}
	// Lastly, if a DBB SMF record, add all of it's information to the table. This is the only record type
	// DBB knows about and is able to handle parsing.
	if (smfRec.getType() == 122 && smfRec.getSubType() == 2)
	{
		def dbbSmfRec = new DBBSmfRecord( data );
		report.append("""
			<td>${dbbSmfRec.getProductName()}</td>
			<td>${dbbSmfRec.getProductFeature()}</td>
			<td>${dbbSmfRec.getProductId()}</td>
			<td>${dbbSmfRec.getUserid()}</td>
			<td>${dbbSmfRec.getVersion()}</td>
			<td>${dbbSmfRec.getRelease()}</td>
			<td>${dbbSmfRec.getMod()}</td>
			<td>${dbbSmfRec.getBuild()}</td>
			<td>${dbbSmfRec.getTimestamp()}</td>""")
	}
	report.append("""
		</tr>""")
}
	