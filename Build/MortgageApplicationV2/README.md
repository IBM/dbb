# MortgageApplicationV2
This folder contains two updated versions of the [MortgageApplication](../MortgageApplication) sample designed to be built by [zAppBuild](../zAppBuild).  Each of the subfolders in MortgageApplicationV2 is intended to be deployed as a stand alone Git repository in order to correctly calculate changed files using Git commands and also to demonstrate how to configure and buid an application whose source files are distributed across multiple Git repositories. 

## Sample Applications

### MortgageApplicaton
This version of the Mortgage application is configured so that all source files are contained in a single repository.

**Example showing how to build all programs in MortgageApplication**
```
$DBB_HOME/bin/groovyz build.groovy --workspace /u/build/repos --application MortgageApplication --outDir /u/build/out --hlq BUILD.MORTAPP --fullBuild
```

### Mortgage-Common
This version of Mortgage application is configured so that the source files are distributed across three repositories:
* Mortgage-Common - contains the configuration files for the Mortgage application as well as common copybooks
* Mortgage-EPSC - contains the source files for the EPCS component of the Mortgage application
* Mortgage-EPSM - contains the source files for the EPSM component of the Mortgage application

**Example showing hot to build all programs in the Mortgage-Common**
```
$DBB_HOME/bin/groovyz build.groovy --workspace /u/build/repos --application Mortgage-Common --outDir /u/build/out --hlq BUILD.MORTCOM --fullBuild
```

See [zAppBuild/BUILD.md](../zAppBuild/BUILD.md) for additional information about building applications using zAppBuild.

## Creating stand alone Git repositories from MortgageApplicationV2 subfolders

### Create a local Git repository for the subfolder
1. Clone the DBB community repository to either a Linux or Windows machine.
2. Copy the subfolder you want to convert to a stand alone repository to another location on the machine outside of the cloned `dbb` local repository.
3. Change directory to the new new directory.
4. Type `git init` to create the local repository.
5. Type `git add *` to add all files in to the local repository.
6. Type `git commit -m "<message>"` to do an initial commit to the local repositry.

### Push the new local repository to a remote central repository.
1. Create the remote repository with the same name as the new directory using the appropriate method for the remote Git server. Examples: GitHub, GitLab, Bitbucket.
2. Change directory to the new directory on the local machine.
3. Type `git remote add origin <remoteGitServerURL>/<directoryName>.git`
3. Type `git push -u origin master`

The new remote repository is now ready to clone to USS.
