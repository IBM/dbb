# Report Migration Sample
### Overview
This sample provides a script to migrate DBB Build Reports containing javascript to static versions. This script is a combination of Unix shell scripts and Apache Groovy scripts.

<pre><b>Any customizations added to the HTML transformer templates will be lost.</b></pre>

NOTE: Due to a known defect, CopyToPDS records will not show up in the generated html. A fix will be available shortly.

### Prerequisites
* DBB Toolkit V1.1.4
    * DBB_HOME environment variable must be set
* IBM Java v8 64bit
    * JAVA_HOME environment variable must be set

NOTE: For migrating Build Reports stored in the DBB 2.x Metadata Store, use the Report Migration Sample located in this repository's `main` branch.

### Folder Content
* bin - Contains the shell scripts that drive the migration process
* groovy - Contains Groovy/DBB scripts that are invoked by the shell scripts
* tests - Contains JUnit functional test cases for the sample, further documentation included within.

### Migration Process
The report migration is a two step process that includes scanning the Repository Client for the requested build groups to create a list of Build Reports to be migrated, and processing the list to generate static Build Reports.

#### Step 1: Create Migration List
The first step is performed by invoking the `create-migration-list.sh` script located in the bin directory. This script takes the migration list destination, Repository Client connection information, and the build groups to convert reports for as input.

The input build groups are matched to those in the Repository Client. Next, the Build Reports for these groups are then filtered to include only those with `</script>` tags in their HTML. This list is then output to a json file to be consumed in the next step.

The generated migration list has the following format:
```
{
    "Group 1": ["result-label-1", "result-label-2", ...]
    "Group 2": ...
}
```
##### Usage:
```
usage: create-migration-list.sh <json-list> [options] [--help]
Using DBB version 1.1.4
 -debug,--debug           Prints groups and reports that are skipped.
 -grp,--grp <arg>         A comma seperated list of build groups with
                          support for wildcard '*' matching.
 -grpf,--grpf <arg>       A file containing build groups seperated by new
                          lines with support for wildcard '*' matching.
 -help,--help             Prints this message.
 -id,--id <arg>           Repository Client user id.
 -pw,--pw <arg>           Repository Client password.
 -pwFile,--pwFile <arg>   Repository Client password file.
 -url,--url <arg>         Repository Client URL. Example:
                          https:<Repository Client location>
```
The following options are required:
* Repository Client Connection options:
    * --id
    * --url and/or --properties
    * --pw or --pwFile
* Group options:
    * --grp or --grpf

#### Step 2: Migrate List
The second step is performed by invoking the `migrate-list.sh` script located in the bin directory. This script takes the previously generated migration list and Repository Client connection information as input. The input list is iterated over, regenerating and uploading the static HTML using the data stored in the result.
```
usage: migrate-list.sh <json-list> [options] [--help]
Using DBB version 1.1.4
 -debug,--debug           Prints entries that are skipped.
 -help,--help             Prints this message.
 -id,--id <arg>           Repository Client user id.
 -pw,--pw <arg>           Repository Client password.
 -pwFile,--pwFile <arg>   Repository Client password file.
 -url,--url <arg>         Repository Client URL. Example:
                          https:<Repository Client location>
```
The following options are required:
* Repository Client Connection options:
    * --id
    * --url and/or --properties
    * --pw or --pwFile