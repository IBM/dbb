
# JCL Migration Sample
### Overview
This sample provides a script to migrate JCL from a Z/OS dataset to a local Groovy script. The sample is a combination of Unix shell scripts, Apache Groovy scripts, templates, and configuration files.

### Prerequisites
* DBB Toolkit
    * DBB_HOME environment variable must be set
* Rocket’s Git and supporting open source tools
* IBM Java v8 64bit
    * JAVA_HOME environment variable must be set

### Folder Content
* bin - Contains the shell script that drives the migration process
* conf - Contains the jclmig.config file that contains optional parameters used by the migration process
* groovy - Contains Groovy/DBB scripts that are invoked by the shell scripts
* template - Contains template for the generated Groovy scripts

### Configuration
Fill in information related to the JCL project in conf/jclmig.config file. The conf/jclmig.config file contains the configuration for the migration process. You can copy, modify, and rename configuration files for different JCL projects but you should use the same configuration file for both steps of the migration process.

**Optional Configuration**
* restrictedPgms: A comma separated list of programs that require APF authorization or other special treatment. These are usually programs like IFJEFT01 which starts a TSO environment. Because these programs cannot be called directly from a Groovy environment, the migration tool will attempt to convert these steps into inline JCL. Warnings will be produced and the generated code should be reviewed before running the generated scripts. Add additional programs as necessary. By default, the list contains IKJEFT01, IKJEFT1A, IKJEFT1B, IRXJCL, and IOEAGFMT.
* tempDataSetOptions: Default BPXWDYN options used to allocate temporary datasets when no other options are specified on the DD Statement. If you have coded SYSOUT=* or DUMMY as your DD statement these options will be used. The default value is 'cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new'.
* procLibs: A comma separated list of PROCLIBs to scan when the JCL being migrated contains EXEC procName statements.

## Migration Process
The JCL migration is a single step process that includes scanning the JCL to produce DBB XML and then converting the XML into a Groovy script using DBB functionality to accomplish the same tasks.  The migration is performed by invoking JCLtoDBB.sh in the bin directory.
```
./JCLtoDBB.sh -d USER.PROJECT.JCL -m MEMBER -p PROJECT -o jclMigration -c ../conf/jclmig.config -s false -g false

usage: JCLtoDBB.groovy [options]
 -c,--configFile <configFile>                       Path to the JCL migration configuration file.  If specified, path is considered absolute if it
                                                    begins with a slash else it is relative path from the migration tool bin directory.  Default is
                                                    ../conf/jclmig.config.
 -d,--dataset <MVS dataset>                         Dataset containing JCL to be migrated
 -g,--genExecVars <Generate executable variables>   Specify true to generate executable variables
 -h,--help                                          Show usage information
 -m,--member <JCL member>                           JCL member being migrated
 -o,--outputDir <output directory>                  Directory in the HFS where all files will be written. If specified, path is considered absolute if
                                                    it begins with a slash else it is relative path from the users home directory.  Default is
                                                    jclMigration.
 -p,--project <JCL project>                         JCL project to be migrated
 -s,--saveOutputs <save JCLExec outputs>            Specify true to generated code to save outputs from a JCLExec


### JCL Migration to DBB XML
This portion of the process will scan the JCL and produce a DBB XML file that represents the steps, programs, and datasets, used in the JCL.


### Groovy Script Generation
The Groovy script generation portion of the process takes the DBB XML file created and generates a shell script and a Groovy script. The generated Groovy script calls DBB functionality to perform the steps done in the original JCL. The shell script is a convenience script to start the Groovy script. 


