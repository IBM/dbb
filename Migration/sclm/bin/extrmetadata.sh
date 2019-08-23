#!/bin/sh

##############################################################################################
#
# This script calls the SclmExtract.groovy and passes in the name of the REXX execs
# to run.
# 'EXTARCH' retrieves ARCHDEF information from SCLM project
# 'EXTMEMB' retrieves member information from SCLM project
#
##############################################################################################

CMD="$DBB_HOME/bin/groovyz ../groovy/SclmExtract.groovy -x EXTARCH ../conf/sclmmig.config"
$CMD

CMD="$DBB_HOME/bin/groovyz ../groovy/SclmExtract.groovy -x EXTMEMB ../conf/sclmmig.config"
$CMD
