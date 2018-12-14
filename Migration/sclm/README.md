
# SCLM Migration Sample
This sample provides scripts to migrate source members to local Git repository and convert the build information in SCLM into build Groovy scripts.

## File Structure
<TODO>

## Pre-requisite
1. Since this sample requires DBB Toolkit, user is required to set up the DBB_HOME environment.
2. Fill in information related to the SCLM project in conf/sclmmig.config file.

## Outline Steps
These steps need to be executed in the exact order as described below:
1. Extract build information and source members into temporary data sets and files on HFS.
2. Generate an zimport shell script.
3. Execute the zimport shell script to migrate source members to local Git repository.
4. Generate build related mapping files.
5. Generate build scripts.

## Extract Information From SCLM
This step involves 3 tasks corresponding to the 3 shell scripts in the 'bin' directory.
Each of the shell scripts calls the same Groovy script in 'SclmExtract.groovy' and pass in
the target REXX execute which is one of the 3 REXX files in the 'rexx' directory.  The 'SclmExtract.groovy'
actually calls this REXX script using ISPFExec API. At the end of each task, user is required
to review the generated files, log and report.

1. Extract Metadata
*Summary* : 
*Action* : Runs the shell script 'extrmetadata.sh'.
*Output* : The following files are generated after a sucessful run:
* *archtype.txt* : 

* *keyref.xml* : 

* *langext.txt* : 

* *members.xml* :

* *projseq.txt* :

*Review* : If this step failed, user is recommended to review the EXTMTDT.log in the 'logs'
directory under the output directory.  If this step succeeded, user is recommended to review
the EXTMTDT_Report.txt file, specially the warnings and errors in this report file. Also user
is recommended to review the 'langext.txt' file to ensure the default type and file extension
are correctly set for each language.

2. Generate System Definitions XML
*Summary* :
*Action* : Runs the shell script 'gensysdefxml.sh'.
*Output* : The following files are generated after a successful run:
* *systemDefinitions.xml* :

* *fileMetaData.xml* :

*Review* : If this step failed, user is recommended to review the GENDEF.log in the 'logs'
directory under the output directory. If this step succeeded, user is recommended to review
the GENDEF_Report.txt file for warnings and errors.  

3. Extract Source Members
*Summary* :
*Action* : Runs the shell script 'extsource.sh'
*Output* : The source members are extracted into temporary data sets. The data sets are 
created in the form of ${HLQ}.Vxx.${GROUP}.${TYPE}, where 'xx' is corresponding to the
versions of the member. A file 'members.txt' is also generated in the output directory
that lists all of the source members along with their associated language definition.
*Review* : If this step failed, user is recommended to review the EXTSRC.log in the 'logs'
directory under the output directory. If this step succeeded, user is recommended to review
the EXTSRC_Report.txt file for warinings and errors.    

You are done with the SCLM extraction when all of the 3 sub-steps completed successfully.

2. Generate zImport Shell Script
This step is making use of the following output files generated in previous steps:
'members.txt'.  



