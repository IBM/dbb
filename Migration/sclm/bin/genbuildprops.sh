#!/bin/sh

##############################################################################################
#
# This script first calls the userinputs.sh to retrieve the user inputs and then calls
# the GenerateMigrationScript.groovy to generate a script to migrate source files
# from temporary data sets to a local GIT repository.
# 
##############################################################################################

CMD="$DBB_HOME/bin/groovyz ../groovy/GenerateBuildProperties.groovy ../conf/sclmmig.config"

$CMD
