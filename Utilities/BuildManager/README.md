# Build Manager
Background process for queueing and managing build scripts without additional JVM overhead.

## Overview
The purpose of the Build Manager is to reduce JVM overhead by providing JVM pooling. It does this by wrapping traditional DBB build scripts (e.g build.groovy) with a generic Build Process, which keeps the JVM alive for reuse by similar build scripts.


 ![Overview of the traditional DBB Build Process](resources/images/traditional_process.png)
 
 ![Overview of the Build Manager as a solution](resources/images/build_manager.png)
 

## Installation
The Build Manager is packaged as a JAR file, which can be found in the *resources* directory. It can be used out-of-the-box, or rebuilt and used.

* Requires at least DBB 1.0.1b90 for StaticMemoryReset()

### Building the JAR 
Use the `build.groovy` script to build the JAR file.
1. Naviate to the BuildManager folder `cd Samples/BuildManager`
2. Edit the `resources/config.properties` file, add a value for `dbb_home` for the Groovy script to use
3. Run the Groovy build script `groovy build.groovy`

### Installing the JAR
Place the dbb.manager.jar file in your dbb_{version}/lib directory.

If it cannot be placed in the DBB/lib directory, the location that it is placed in needs to be added to `build_process.groovyz.preload.classpath=` in config.properties.

## Configuration
The BuildDaemon requires a configuration file to start.
### config.properties
Update the resources/config.properties file to fit your environment. 
```

daemon_port=6789
dbb_home=
java_home=

# GROOVY PRELOADS
build_process.groovyz=daemon.GroovyBuildProcess
build_process.groovyz.command=groovyz # which process you want to preload (groovyz only at this time)
build_process.groovyz.preload.amount=1 # how many processes you want to preload
build_process.groovyz.preload.classpath= # string to append to classpath
build_process.groovyz.preload.options= # string to append to classpath
```
At this time, the BuildDaemon only supports preloading for Build Processes. 

### client.sh
Update the hostName and port values in resources/client.sh to fit your system. The term "localhost" will cause issues, so use 127.0.0.1 or equivalent.

## Components
### Build Client (build.client)
The client used to interact with the Build Daemon by sending a message to a socket port.

To use the client, the shell script must be somewhere on your system. The script requires bash, we recommend using Rocket Bash for Z/OS, which comes installed with IBM Dependency Based Build.

#### Calling the Client
`/var/rocket/bin/bash client.sh {command} {target: optional}`
* Command: Expects `-kill`, or `groovyz`
* Target: The groovy file, expected to be passed when using the `groovyz` command.
 
#### Example Usage
`/var/rocket/bin/bash client.sh groovyz /u/george/MortgageApplication/build/build.groovy`

`/var/rocket/bin/bash client.sh -kill` (stop the Build Daemon & Build Processes)


### Build Daemon (build.daemon)
The Build Daemon manages the Build Process instances, and dispatches commands. Build Processes are preloaded from the specifications in the configuration file.

#### Example Usage
`/usr/lpp/java/J8.0_64/bin/java -classpath com.ibm.dbb.manager.jar build.daemon.BuildDaemon /u/kporter/Daemon/config.properties`

*Please note that the Build Daemon requires a configuration file argument. The Daemon is expected to be a background process.*

### Build Process (build.process)
The Build Process is a generic Java program that runs Groovy scripts. Its purpose is to keep the JVM alive.

