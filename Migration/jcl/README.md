
# JCL Migration Sample
### Overview
This sample provides scripts to migrate JCL from a Z/OS dataset to a local Groovy script. The sample is a combination of Unix shell scripts, Apache Groovy scripts, templates, and configuration files.

### Prerequisites
* DBB Toolkit
    * DBB_HOME environment variable must be set
* Rocket’s Git and supporting open source tools
* IBM Java v8 64bit
    * JAVA_HOME environment variable must be set

### Folder Content
* bin - Contains the shell scripts that drive the migration process
* conf - Contains the jclmig.config file that must be edited before the migration process is started
* groovy - Contains Groovy/DBB scripts that are invoked by the shell scripts
* template - Contains template for the generated Groovy scripts

### Configuration
Fill in information related to the JCL project in conf/jclmig.config file. The conf/jclmig.config file contains the configuration for the migration process. You can copy, modify, and rename configuration files for different JCL projects but you should use the same configuration file for both steps of the migration process.

**Required Configuration**
* proj: Project name for this configuration. The project name is used in managing output from the migration process.

**Optional Configuration**
* outputDir: The directory in the HFS where all files will be written. The directory will have a sub-directory created called jclMigration/{proj}. outputDir defaults to the users home directory.
* restrictedPgms: A comma separated list of programs that require APF authorization or other special treatment. These are usually programs like IFJEFT01 which starts a TSO environment. Because these programs cannot be called directly from a Groovy environment, the migration tool will attempt to convert these steps into inline JCL. Warnings will be produced and the generated code should be reviewed before running the generated scripts. Add additional programs as necessary. By default, the list contains IKJEFT01, IKJEFT1A, IKJEFT1B, IRXJCL, and IOEAGFMT.
* tempDataSetOptions: Default BPXWDYN options used to allocate temporary datasets when no other options are specified on the DD Statement. If you have coded SYSOUT=* or DUMMY as your DD statement these options will be used. The default value is 'cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new'.
* saveJCLOutputs: Specify true to generate code to save outputs from a JCLExec. The default is false.

## Migration Process
The JCL migration is a two step process that includes scanning the JCL to produce DBB XML and then converting the XML into a Groovy script using DBB functionality to accomplish the same tasks.

### JCL Migration to DBB XML
This step will scan the JCL and produce a DBB XML file that represents the steps, programs, and datasets, used in the JCL. You can use gendbbxml.sh in the bin directory to invoke the Groovy migration scripts.
```
./gendbbxml.sh  USER.PROJECT.JCL  MEMBER -c ../conf/jclmig.config
```
USER.PROJECT.JCL and MEMBER are the dataset and member of the JCL to be migrated. Optionally, use the -c option to specify the configuration for this migration. 

### Groovy Script Generation
The Groovy script generation takes the DBB XML file created by the previous step and generates a shell script and a Groovy script. The generated Groovy script calls DBB functionality to perform the steps done in the original JCL. The shell script is a convenience script to start the Groovy script. You can use genbuildscripts.sh in the bin directory to invoke the Groovy script generation.
```
./genbuildscripts.sh -c ../conf/jclmig.config
```
Use the -c option to specify the configuration for this migration. This should be the same configuration file used in the previous step. It will use the proj and outputDir configuration options to find the DBB XML file to produce the Groovy script.

