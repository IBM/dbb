#!/bin/sh

##############################################################################################
#
# This script calls the SclmExtract.groovy and passes in the name of the REXX execute
# 'EXTSRC' to run. The 'EXTSRC' restores the editable source members from SCLM project
# to temporary data sets
#
##############################################################################################

CMD="$DBB_HOME/bin/groovyz ../groovy/SclmExtract.groovy -x MEMCNT ../conf/sclmmig.config"

$CMD

CMD="$DBB_HOME/bin/groovyz ../groovy/SclmExtract.groovy -x EXTSRC ../conf/sclmmig.config"

$CMD
