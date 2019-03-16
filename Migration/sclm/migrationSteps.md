# Migration Process
The migration process is comprised of three phases each of which contain multiple steps.  
* SCLM Extraction
* Source Code Migration
* Build Script Generation

The phases and steps need to be executed in the order presented here as output from one step may be required input in a later step.

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
    * `<outputDir>/langext.txt` - Verify the default type and file extension are correctly set for each language. When **allLangs** is set to 'true' in the sclmMig.config file, the list in this file could contain language definitions that do not have default type. The tool will generate the Groovy scripts for these language definitions but these scripts will not be referenced by any source files.
    * `<outputDir/logs/EXTMTDT.log` - (if error). 

### Step 1-2 : Generate System Definitions XML
This step produces output that will be required to generate DBB XML.
* *Action* : Execute shell script `bin/gensysdefxml.sh`
* *Output* : The following files are generated during a successful run:
    * `<outputDir>/systemDefinitions.xml`
    * `<outputDir>/fileMetaData.xml`
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<outputDir>/GENDEF_Report.txt` - Note any warnings or errors in this report file. 
    * `<outputDir/logs/GENDEF.log` - (if error).

### Step 1-3 : Extract Source Members
This step extracts source members from SCLM and copies them to temporary datasets in preparation for migrating them to a Git local repository.
* *Action* : Execute shell script 'extrsource.sh'
* *Output* : The source members are extracted into temporary data sets. The data sets are created in the form of ${HLQ}.Vxx.${GROUP}.${TYPE}, where 'xx' is corresponding to the versions of the member. A file 'members.txt' is also generated in the output directory that lists all of the source members along with their associated language definition.
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<outputDir>/EXTSRC_Report.txt` - Note any warnings or errors in this report file. 
    * `<outputDir/logs/EXTSRC.log` - (if error). 

## Phase 2 : Source Code Migration
This phase starts by generating a source migration shell script that invokes the DBB migration tool multiple times to copy SCLM source members from the temporary datasets creating during the last phase to a local Git repository on zFS.  A Git commit is executed after each move.

### Step 2-1 :  Generate Source Migration Shell Script
This step generates the source code migration shell script .
* *Action* : Execute shell script `bin/genzimport.sh`
* *Output* : The following files are generated during a successful execution:
    * `<outputDir>/zimport.sh`
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<outputDir>/zimport.sh` - Verify the DBB migration tool calls and Git commands for each SCLM version to migrate. 
    
### Step 2-2 :  Execute Source Migration Shell Script
This step creates a local Git repository on zFS and then migrates SCLM source members from temporary datasets created in a previous step to the local repository.
* *Action* : Execute shell script `<outputDir>/zimport.sh` generated in the previous step
* *Output* : The source members are copied from the temporary data sets to subfolders in the local Git repository according to the mapping rules in the `<outputDir>/zimport.sh` shell script. Additionally a `<repo>/.gitattributes` file is generated for automatic code page conversion.
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<repo>/.gitattributes` - Verify that the statements have the correct code page for the working tree encodings. 

## Phase 3 : Build Script Generation
This phase will use metadata generated in *Phase 1 : SCLM Extraction* to generate Groovy build script stubs from SCLM build process definitions.

### Step 3-1 : Generate Build Properties
This step generates the properties files required by Groovy build scripts. 
* *Action* : Execute shell script `bin/genbuildprops.sh`
* *Output* : The following files are generated during a successful execution:
    * `<repo>/build/build.properties` contains all required and optional settings for running a build.
    * `<repo>/build/datasetMappings.properties` contains mappings of datasets and source directories.
    * `<repo>/build/files.properties` contains file level override properties.
    * `<repo>/build/files.txt` contains a list of files for a full build.
    * `<repo>/build/scriptMappings.properties` contains mappings of Groovy script and source files.            
* *Review* : The following files should be reviewed before proceeding to the next step:
    * `<repo>/build/build.properties` some of the required settings need to be filled in before running a test build.
    
### Step 3-2 : Generate DBB XML    
This step generates the DBB XML based on the previous generated System Definitions XML.
* *Action* : Execute shell script `bin/gendbbxml.sh`          
* *Output* : The following files are generated during a successful execution:
    * `<outputDir>/dbb.xml
* *Review* : None.

### Step 3-3 : Generate Groovy Scripts
This step generates Groovy scripts based on the DBB XML.  The scripts are generated and stored in the <repo>/build directory.
* *Action* : Execute shell script `bin/genbuildscripts.sh`
* *Output* : An overall build Groovy script will be generated as well as scripts for each element &lt;script&gt; in DBB XML. Also a shell script build.sh is generated for testing purposes.
* *Review* : The build order specified in the overall build script should be reviewed carefully. All generated scripts resemble the definitions in SCLM but manual intervention is expected.

## Phase 4 : Run a Test Build
DBB server and toolkit are required to run a test build. In addition, the `<repo>/build/build.properties` need to be completed.  To run a test build, execute the shell script `<repo>/build/build.sh`.
     