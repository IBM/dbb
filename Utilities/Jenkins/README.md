# Jenkins Utility Scripts
These sample utility scripts are supplied to address issues when running Jenkins remote agents on z/OS UNIX System Services (USS).

`gitenv.sh` - A convinient shell script that sets all the environment variables required by Rocket's Git port for USS.  Add the shell script to the Jenkins remote agent configuration's prefix start command to automatically set the environment variables. **IMPORTANT!** *The shell script requires configuration.  See the comment on how to modify the script for your system.*

`git-jenkins.sh` - Resolves two encoding issues when using the Git client plugin that is part of the Jenkins Freestyle project. 
1. The plugin issues a git rev-parse command in which some of the output from the command is encoded in UTF-8 instead of EBCDIC.  The shell script pipes all command output through ICONV. 
2. The plugin creates a temporary shell scripts to handle SSH communication with Git.  However these shell scripts are encoded in UTF-8 and need to be converted to EBCDIC to be invoked.

Use the `git-jenkins.sh` script by overriding the Git tool location with this script in the Node Properties of the remote agent configuration.

