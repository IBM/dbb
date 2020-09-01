# Jenkins Utility Scripts
These sample utility scripts are supplied to address issues when running Jenkins remote agents on z/OS UNIX System Services (USS).
## Rocket git environment
`gitenv.sh` - A convinient shell script that sets all the environment variables required by Rocket's Git port for USS.  Add the shell script to the Jenkins remote agent configuration's prefix start command to automatically set the environment variables. **IMPORTANT!** *The shell script requires configuration.  See the comment on how to modify the script for your system.*

## Git credentials to access central git provider
The Jenkins Git client plugin is used to specify the gitcredentials used in the communication with the central git provider. When managing the credentials within Jenkins, temporary files are written to the build workspace.

Please see the available scripts to address encoding issues depending on the version of the Jenkins Git client plugin.

`git-jenkins.sh` - Resolves two encoding issues when using the *Jenkins Git client plugin < v3.0* that is part of the Jenkins Freestyle project:
1. The plugin issues a git rev-parse command in which some of the output from the command is encoded in UTF-8 instead of EBCDIC.  The shell script pipes all command output through ICONV. 
2. The plugin creates a temporary shell scripts to handle SSH communication with Git.  However these shell scripts are encoded in UTF-8 and need to be converted to EBCDIC to be invoked.

`git-jenkins2.sh` resolves encoding issues with the *Jenkins Git client plugin v3.0 onwards* for connecting to the central git provider via HTTP or SSH keys, while

`git-jenkins3.sh` does the same for *Jenkins Git client plugin < v3.0*, like 2.7.x or 2.8.x. 

1. The plugin creates temporary shell scripts to handle the HTTP communication with the central Git server - but in different ways which cause the different scripts. These temporary files are encoded in UTF-8 and need to be converted to EBDCDIC. This script enables to use credentials managed in Jenkins to handle HTTP(s) connections to the central Git provider.
2. In the case of managing SSH keys and SSH passphrases in Jenkins to handle the communication with the central Git server, the plugin creates temporary files like in the first case for http communication. The script enables to use a SSH passphrase managed in Jenkins for connections to the git provider.

Use the `git-jenkins.sh`,  `git-jenkins2.sh` or the `git-jenkins3.sh` script by overriding the Git tool location with this script under the Node Properties of the remote Jenkins agent configuration.

Starting with Git client plugin 3.4.1, the plugin introduced a set of new environment variables, to specify the file encoding of the temporary files, which make the above git-jenkins scripts obsolete.

More about this topic, can be found in this IBM Technote: [zDevops: Managing git credentials in Jenkins to access the central git provider](https://www.ibm.com/support/techdocs/atsmastr.nsf/WebIndex/TD106439) 