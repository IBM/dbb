# Creating a zAppBuild stand alone Git repository
zAppBuild is intended to be maintained as a stand alone Git repository by the release engineering team of a corporation or  organization.  It is intended to be cloned to a single location on USS to be used to build all of your z/OS applications.

## Create a local Git repository for zAppBuild
1. Clone the DBB community repository to either a Linux or Windows machine.
2. Copy the `Build/zAppBuild` folder to another location on the machine outside of the cloned `dbb` local repository.
3. Change directory to the new zAppBuild directory.
4. Type `git init` to create the local repository.
5. Type `git add *` to add all files to the local repository.
6. Type `git commit -m "<message>"` to do an initial commit to the local repositry.

## Push the new local repository to a remote central repository.
1. Create the remote repository called "zAppBuild" using the appropriate method for the remote Git server. Examples: GitHub, GitLab, Bitbucket.
2. Change directory to the new zAppBuild directory on the local machine.
3. Type `git remote add origin <remoteGitServerURL>/zAppBuild.git`
3. Type `git push -u origin master`

The zAppBuild remote repository is now ready to clone to USS.
