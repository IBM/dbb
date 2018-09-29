# Samples/Utilities Folder
The Utilities folder is comprised of additional scripts to provided additional capabilities of IBM's Dependency Based Build. Currently, the scripts provided in this folder are:
* Pruner.groovy: allows the user to prune build results in the DBB Repository Web Application.
* Saver.groovy: allows the user to toggle a save property in build results on the DBB Repository Web Application so that thwy cannot be pruned.


## Configuring Utilities for your system
The Utilities folder contains a properties file that needs to be edited before some scripts can be executed. The instructions on what needs to be updated are included within the properties file.
* user.properties - Contains credential properties to allow the user to connect to the DBB Repository Web Application
 
### Pruner.groovy
This script allows a user to prune specific build results based on the user's inputs. The supported options are:
-a,--all
: will prune all builds in all groups

-d,--id <ID>
:will prune the build with the specified ID

-g,--group <group name>
: will prune all the builds in the specified group
Optional specifiers:
-c,--cleanBuildsToKeep
: will prune all clean builds in the specified group
-e,--errorBuildsToKeep
: will prune all error builds in the specified group
-w,--warningBuildsToKeep
: will prune all warning builds in the specified group
-l,--labelName <label name>
: will prune the build in the specified group with the specified label

-o,--oldest <number of builds to keep>
:will save the most recent specified number of builds, the rest will be pruned
Optional specifiers:
-g,--group <group name>
: will limit prune to the specified group
-c,--cleanBuildsToKeep <number of clean builds to keep>
:will save the most recent specified number of clean builds, the rest will be pruned
-e,--errorBuildsToKeep <number of error builds to keep>
:will save the most recent specified number of error builds, the rest will be pruned
-w,--warningBuildsToKeep <number of warning builds to keep>
:will save the most recent specified number of warning builds, the rest will be pruned
Note that if -c, -e, or -w is used, then no number needs to be entered with the -o option

-i,--interactive
: will launch an interactive mode with the same possible options as above
To test the result of any option and not make any changes, add -p/--preview.
To view a description of available options, use -h/--help

### Saver.groovy
This script allows a user to toggle a save flag to prevent pruning for specific build results based on the user's inputs. The supported options are:
-a,--all
: will save all builds in all groups

-d,--id <ID>
:will save the build with the specified ID

-g,--group <group name>
: will save all the builds in the specified group
Optional specifiers:
-c,--cleanBuildsToKeep
: will save all clean builds in the specified group
-e,--errorBuildsToKeep
: will save all error builds in the specified group
-w,--warningBuildsToKeep
: will save all warning builds in the specified group
-l,--labelName <label name>
: will save the build in the specified group with the specified label

-o,--oldest <number of builds to keep>
:will save the most recent specified number of builds
Optional specifiers:
-g,--group <group name>
: will limit save to the specified group
-c,--cleanBuildsToKeep <number of clean builds to keep>
:will save the most recent specified number of clean builds
-e,--errorBuildsToKeep <number of error builds to keep>
:will save the most recent specified number of error builds
-w,--warningBuildsToKeep <number of warning builds to keep>
:will save the most recent specified number of warning builds
Note that if -c, -e, or -w is used, then no number needs to be entered with the -o option

-i,--interactive
: will launch an interactive mode with the same possible options as above
To test the result of any option and not make any changes, add -p/--preview.
To view a description of available options, use -h/--help
