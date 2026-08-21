# Samples/ReadSMFRecords Folder
The ReadSMFRecords folder contains scripts to read System Management Facilities (SMF) records using IBM's Dependency Based Build capabilities. The script can read and process many types
of SMF records. However, only the DBB SMF record (Type:122, subType:2) is fully parsed and recorded in the report. Only the header portion of other types will be recorded in the report.

## Configuring
The smf.properties is loaded by the read SMF script to provide default values for several parameters. Script parameters will override these values.
* hlq - High level qualifier for a working data set. The script will dump the SMF records to this data set and then read and process the records.
* recordType - The SMF record type to process. This can be a single number or a range (i.e. 0:255).
* datasets - The SMF datasets, a comma separated list, as defined in SYS1.PARMLIB(SMFPRMxx) DSNAME option.
 
### ReadSMF.groovy
This script allows a user to read SMF records, specifically the DBB SMF record. The supported options are:
<table>
<tr>
 <td valign="top">-d,--datasets &lt;datasets&gt;</td><td valign="top">Comma separated list of SMF datasets as defined in SYS1.PARMLIB(SMFPRMxx) DSNAME option</td>
</tr>
<tr>
 <td valign="top">-e,--logEncoding &lt;encoding&gt;</td><td valign="top">Encoding of output logs. Default is EBCDIC</td>
</tr>
<tr>
 <td valign="top">-h,--help</td><td valign="top">help</td>
</tr>
<tr>
 <td valign="top">-q,--hlq &lt;hlq&gt;</td><td valign="top">High level qualifier for working dataset.</td>
</tr>
<tr>
 <td valign="top">-t,--type &lt;type&gt;</td><td valign="top">SMF type to process. Can be a number or range (i.e. 0:255)</td>
</tr>
<tr>
 <td valign="top">-w,--workDir &lt;dir&gt;</td><td valign="top">Absolute path to the build output directory</td>
</tr>
</table>
