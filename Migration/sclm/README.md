
# SCLM Migration Sample
This sample provides scripts to migrate source members from IBM Software Configuration and Library Manager (SCLM) to a local Git repository. Additionally an update is planned for January 2019 to also include scripts to generate Groovy/DBB build script stubs (may require configuration) from SCLM build definitions.

## Overview
* The sample is a combination of Unix shell scripts, Apache Groovy scripts and REXX scripts
* Uses the existing [DBB migration tool](https://www.ibm.com/support/knowledgecenter/SS6T76_1.0.2/migration.html)
* Users can choose to how many versions to migrate thus preserving change history 
* The migration process is comprised of three phases:
    * SCLM Extraction
    * Source Code Migration
    * Build Script Generation (Coming Soon)

## Folder Content
* bin - Contains the shell scripts that drive the migration process
* conf - Contains the sclmmig.config file that must be edited before the migration process is started
* groovy - Contains Groovy/DBB scripts that are invoked by the shell scripts
* rexx - Contains REXX scripts used to extract SCLM metadata

## Prerequisites
* DBB Toolkit
    * DBB_HOME environment variable must be set
* Rocket’s Git and supporting open source tools
* IBM Java v8 64bit
    * JAVA_HOME environment variable must be set
* ISPF Legacy Gateway

## Configuration
* Fill in information related to the SCLM project in conf/sclmmig.config file.

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



