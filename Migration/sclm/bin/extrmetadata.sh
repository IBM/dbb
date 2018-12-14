#!/bin/sh

##############################################################################################
#
# This script calls the SclmExtract.groovy and passes in the name of the REXX execute
# 'EXTMTDT' to run.  The 'EXTMTDT' retrieves information from SCLM project
# 
##############################################################################################

CMD="$DBB_HOME/bin/groovyz ../groovy/SclmExtract.groovy -x EXTMTDT ../conf/sclmmig.config"

$CMD
