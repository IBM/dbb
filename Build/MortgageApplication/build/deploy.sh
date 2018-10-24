#!/bin/sh
##############################################################################################
##
##  This sample shell script is provided as an easy way to invoke the sample deploy script
##  from the command line. 
##
##  usage: deploy.sh 
## 
##  **NOTE - Build the application first to populate the work directory with build reports.
##           The deploy scripts use the information in build report to create a ship list 
##           and then import a version to an UrbanCode Deploy component.  
##           UrbanCode Deploy agent must be installed and configured to import versions. 
##
###############################################################################################

# Set the DBB bin directory
DBB_HOME=

# $DBB_HOME/bin/groovyz automatically sets the env variables and classpath required for DBB
CMD="$DBB_HOME/bin/groovyz deploy.groovy $@"

echo $CMD
$CMD
