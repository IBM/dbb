#!/bin/sh

# Set the DBB bin directory
DBB_HOME=
export DBB_HOME

# $DBB_HOME/bin/groovyz automatically sets the env variables and classpath required for DBB
CMD="$DBB_HOME/bin/groovyz Compile.groovy"

$CMD