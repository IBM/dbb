# Report Migration Tests
### Overview
This folder contains JUnit5 functional tests that covers the scripts standard use cases.

### Prerequisites
* DBB Toolkit V1.1.4
    * DBB_HOME environment variable must be set
* IBM Java v8 64bit
    * JAVA_HOME environment variable must be set

NOTE: For migrating Build Reports stored in the DBB 2.x Metadata Store, use the Report Migration Sample located in this repository's `main` branch.

### Folder Content
* samples - Contains sample data for the test cases

### Test Execution
Tests may be executed with the `test.groovy` script located within this folder.
These tests use the following collection names which should be edited if they are already present in your Metadata Store:
* Static-Report-Migration-Test
* Static-Report-Migration-Test-2
```
usage: $DBB_HOME/bin/groovyz test.groovy --id CLIENT-ID --url CLIENT-URL --pwFile CLIENT-PASSWORD-FILE [--help]
 -help,--help             Prints this message.
 -id,--id <arg>           Test Repository Client user id.
 -pwFile,--pwFile <arg>   Test Repository Client user password file.
 -url,--url <arg>         Test Repository Client URL.
```