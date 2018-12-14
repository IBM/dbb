#!/bin/sh

##############################################################################################
#
# This script calls the SclmExtract.groovy and passes in the name of the REXX execute
# 'GENDEF' to run. The 'GENDEF' generates the systemDefinitions.xml from the information
# extracted from SCLM project
# 
##############################################################################################

CMD="$DBB_HOME/bin/groovyz ../groovy/SclmExtract.groovy -x GENDEF ../conf/sclmmig.config"

$CMD
