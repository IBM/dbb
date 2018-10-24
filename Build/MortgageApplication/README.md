# Mortgage Application Sample
The Mortgage Application sample is comprised of a COBOL application and Groovy build scripts that use IBM Dependency Based Build (DBB) APIs to build the application for execution on z/OS. The sample demonstrates the following build functions:
* How to use property files for build configuration
* How to handle build script command line arguments
* How to organize build scripts into main and called scripts for building specific source file types
* How to create new datasets, copy zFS files to datasets, invoke z/OS compilers and linkers, copy SYSPRINT datasets to zFS as log files
* How to scan source files to gather dependency data and store in the DBB repository for later dependency resolution
* How to generate a build report that contains a summary of source files built
* How to store build results in the DBB repository for later retrieval
* How to perform incremental builds by identifying changed files since the last build and discover impacted programs that need rebuilding
* How to run a post build process to create an UrbanCode Deploy shiplist from a saved DBB build report 
* How to build a DB2 application package for SQL programs.
* How to improve performance by caching Groovy scripts

## Configuring the Mortgage Application Sample for your system
All of the build files are located in the `MortgageApplication/build` folder.  The folder contains two properties files that need to be edited before the Mortgage Application build scripts can run successfully.  The files contain instructions on what needs to be set.
* datasets.properties - Contains build properties for Partition Data Sets (PDS) used by Mortgage Application sample scripts
* build.properties - Contains default sandbox properties used by Mortgage Application build scripts.  Many of these properties can be overridden using command line arguments when running the Groovy build script (see the comment block in build.groovy)
* bind.properties - Contains build properties used by BindPackage.groovy to build a DB2 application package for SQL programs in the MortgageApplication sample.
 
## Build Script Organization
The Mortgage Application build folder contains three main build scripts and six called build scripts. The Groovy scripts are extensively commented and can be used to jump start writing build scripts for new applications.

### build.groovy 
This is the main build script for building the Mortgage Application.  It performs build process initiation, scans the source files for dependency data, invokes called build scripts to build specific file types, generates a build report and saves build results to the DBB web application repository. The five called build scripts are 
* BMSProcessing.groovy - Script that demonstrates how to build CICS BMS maps
* Compile.groovy - Script that demonstrates how to just compile a COBOL program to produce a object deck
* LinkEdit.groovy - Script that demonstrates how to link-edit object decks to produce a load module
* CobolCompile.groovy - Script that demonstrates how to compile and link-edit COBOL programs in one script
     * BindPackage.groovy - Script that demonstrates how to build a DB2 application package for SQL programs. Called from CobolCompile.groovy.
* MFSGENUtiltiy.groovy - Script that demonstrates how to build IMS Message Format Service (MFS) files
* Tools.groovy - Script that provides common utility Groovy methods used by the build scripts.

The easiest way to invoke the build.groovy file from the Unix System Services (USS) command line is to use the build.sh shell script provided in the MortgageApplication/build folder. **NOTE: The build.sh shell script may need to be edited if the DBB installation is not located at /usr/lpp/IBM/dbb .** Invoking `build.sh` with no arguments will result in all the files listed in `MortgageApplication/build/files.txt` being scanned and built.  Invoking `build.sh --help` will display the command line arguments that are supported by build.groovy.

The build.groovy script can also be invoked from a Jenkins process server via a remote agent running on USS. It is important to note that when invoking Groovy scripts that use DBB APIs either by using Groovy directly or a tool like Jenkins that invokes Groovy, you will need to set both the Java classpath and the Java native library path to include the DBB toolkit lib directory.  Example:
```
Class path : /usr/lpp/IBM/dbb/lib/* 
Java opts : -Djava.library.path=/usr/lpp/IBM/dbb
```

### impacts.groovy
The impacts.groovy script is an optional pre-build script that runs before build.groovy runs.  When provided with the current build's Git hash, it will retrieve the last successful build's Git hash from the DBB web application repository and execute a `git diff` command to identify which files have changed since the last build. It then runs an impact analysis for each changed file to see what programs will need to be rebuilt. Finally it generates a buildList.txt file that can be passed to build.groovy to build just the programs that need to be built.

In order to use impacts.groovy with build.groovy to changes need to be made to the build.groovy argument list:
* add option `--buildHash <hash>` where <hash> is the Git commit hash for the current build
* use `<workDir>/buildList.txt` as the build list file (last argument) where <workDir> is the directory where the buildList.txt was generated from running impacts.groovy.

Since impacts.groovy requires the current Git build hash as an argument, no shell script has been provided to run the script as it is assumed that it will be run by a build processing server like Jenkins. The Jenkins Git client plugin sets the current build Git hash in environment variable $GIT_COMMIT.

### deploy.groovy 
The deploy.groovy script is an optional post-build script that runs after build.groovy runs.  The script parses the buildReport.json file generated by the build.groovy script at the end of the build.  From the build report it generates an UrbanCode Deploy (UCD) shiplist.  It then runs a UCD buztool command to create a UCD z/OS component version for deployment.

## Build Script Caching (new for DBB v1.0.2)
The Mortgage Application sample is organized so that each program type has its own compile/link-edit Groovy script that gets invoked from the main build.groovy script.  During runtime the Groovy scripts are compiled on the fly into Java classes and invoked.  Though the compilation is very fast, it still takes time. Build processes generally just call the same few Groovy scripts over and over again and if the build process builds hundreds or even thousands of source files this can result in significant (and unnecessary) build processing times.  To help reduce the amount of time it takes to recompile Groovy scripts, DBB provides a Groovy class that customers can extend as part of their build scripts that would automatically load and cache called scripts.

### Design
Users can extend their build scripts by adding the following line to the top of the script:
```
@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
``` 
This now provides the following built-in methods that the script can now invoke:
* `void runScript(File script, String[] args=[])` - This method is used to call another top-down script.
    * Will automatically try to use the cached version of the compiled script class or updates the cache if the script is being called for the first time during the build process.
    * Can be used to call both DBB ScriptLoader based Groovy scripts and generic Groovy scripts
* `void runScript(File script, Map<String,Object> argMap)` - This method is similar to the previous method but passes an argument Map instead of a String array.
    * For use only when calling another DBB ScriptLoader based Groovy script
        * Allows for the easy passing of objects between scripts
        * A Map property called `argMap` is created in the target script
    * If the target script is not a DBB ScriptLoader based Groovy script, then an empty String [] args is passed instead.
* `GroovyObject loadScript(File script)` - This method performs automatic caching similar to the other methods described but only loads the script without trying to run it. 
    * This method can be used to load Groovy scripts that are object oriented i.e. Tools.groovy
* `getScriptDir()` - Returns the String representation of the parent directory of the running script.

**Note** that the `File script` argument in the above methods supports both absolute and relative paths.  If the script parameter contains a relative path, then it is automatically appended to the current script's parent directory.  This means that `loadScript(new File("Tools.groovy"))` will try to load the Tools.groovy script file in the same directory that the current running script is in.
 