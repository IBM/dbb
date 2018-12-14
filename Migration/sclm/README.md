
# SCLM Migration Sample
This sample provides scripts to migrate source members from IBM Software Configuration and Library Manager (SCLM) to a local Git repository. Additionally an update is planned for January 2019 to also include scripts to generate Groovy/DBB build script stubs (may require configuration) from SCLM build definitions.

## Overview
* The sample is a combination of Unix shell scripts, Apache Groovy scripts and REXX scripts
* Uses the existing [DBB migration tool](https://www.ibm.com/support/knowledgecenter/SS6T76_1.0.2/migration.html)
* Users can choose to how many versions to migrate thus preserving change history 

## Assumptions
Please review the [SCLM Assumptions documentation](https://github.com/IBM/dbb/blob/master/Migration/sclm/sclmAssumptions.md)  before running the migration process.

## Prerequisites
* DBB Toolkit
    * DBB_HOME environment variable must be set
* Rocket’s Git and supporting open source tools
* IBM Java v8 64bit
    * JAVA_HOME environment variable must be set
* ISPF Legacy Gateway

## Folder Content
* bin - Contains the shell scripts that drive the migration process
* conf - Contains the sclmmig.config file that must be edited before the migration process is started
* groovy - Contains Groovy/DBB scripts that are invoked by the shell scripts
* rexx - Contains REXX scripts used to extract SCLM metadata

## Configuration
* Fill in information related to the SCLM project in conf/sclmmig.config file.

# Migration Process
The migration process is comprised of three phases each of which contain multiple steps.  
* SCLM Extraction
* Source Code Migration
* Build Script Generation (Coming Soon)

The phases and steps need to be executed in the order presented here as output from one step may be reqired input in a later step.

## Phase 1 : SCLM Extraction
This phase involves the execution of three shell scripts in the 'bin' directory. Each of the shell scripts calls `groovy/SclmExtract.groovy` passing in a target REXX script to execute in the 'rexx' directory.  At the end of each task, the user is required
to review the generated files, logs and reports.

### Step 1-1 :  Extract Metadata
This step interrogates the SCLM repository metadata and produces several output files that are used in later steps. 
* *Action* : Execute shell script `bin/extrmetadata.sh`
* *Output* : The following files are generated during a successful execution:
    * `<outputDir>/archtype.txt`
    * `<outputDir>/keyref.xml` 
    * `<outputDir>/langext.txt` 
    * `<outputDir>/members.xml`
    * `<outputDir>/projseq.txt`
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<outputDir>/EXTMTDT_Report.txt` - Note any warnings or errors in this report file. 
    * `<outputDir>/langext.txt` - Verify the default type and file extension are correctly set for each language.
    * `<outputDir/logs/EXTMTDT.log` - (if error).

### Step 1-2 : Extract Source Members
This step extracts source members from SCLM and copies them to temporary datasets in preparation for migrating them to a Git local repository.
* *Action* : Execute shell script 'extsource.sh'
* *Output* : The source members are extracted into temporary data sets. The data sets are created in the form of ${HLQ}.Vxx.${GROUP}.${TYPE}, where 'xx' is corresponding to the versions of the member. A file 'members.txt' is also generated in the output directory that lists all of the source members along with their associated language definition.
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<outputDir>/EXTSRC_Report.txt` - Note any warnings or errors in this report file. 
    * `<outputDir/logs/EXTSRC.log` - (if error).  

### Step 1-3 : Generate System Definitions XML (Optional at this time)
This step produces output that will be required in *Phase 3 - Build Script Generation* that should be completed early 2019. 
* *Action* : Execute shell script `bin/gensysdefxml.sh`
* *Output* : The following files are generated during a successful run:
    * `<outputDir>/systemDefinitions.xml`
    * `<outputDir>/fileMetaData.xml`
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<outputDir>/GENDEF_Report.txt` - Note any warnings or errors in this report file. 
    * `<outputDir/logs/GENDEF.log` - (if error).

## Phase 2 : Source Code Migration
This phase starts by generating a source migration shell script that invokes the DBB migration tool multiple times to copy SCLM source members from the temporary datasets creating during the last phase to a local Git repository on zFS.  A Git commit is executed after each move.

### Step 2-1 :  Generate Source Migration Shell Script
This step generates the source code migration shell script .
* *Action* : Execute shell script `bin/genzimport.sh`
* *Output* : The following files are generated during a successful execution:
    * `<outputDir>/migrate-<prod>.sh`
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<outputDir>/migrate-<prod>.sh` - Verify the DBB migration tool calls and Git commands for each SCLM version to migrate . 
    
### Step 2-2 :  Execute Source Migration Shell Script
This step creates a local Git repository on zFS and then migrates SCLM source members from temporary datasets created in a previous step to the local repository.
* *Action* : Execute shell script `<outputDir>/migrate-<prod>.sh` generated in the previous step
* *Output* : The source members are copied from the temporary data sets to subfolders in the local Git repository according to the mapping rules in the `<outputDir>/migrate-<prod>.sh` shell script. Additionally a `<repo>/.gitattributes` file is generated for automatic code page conversion.
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<repo>/.gitattributes` - Verify that the statements have the correct code page for the working tree encodings. 

## Phase 3 : Build Script Generation *(Coming Soon!)*
This phase will use metadata generated in *Phase 1 : SCLM Extraction* to generate Groovy build script stubs from SCLM build process definitions.  Expected to be completed sometime in January 2019.

