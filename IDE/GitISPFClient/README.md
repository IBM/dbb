# Git ISPF Client

Table of Contents

<!-- TOC -->

- [Git ISPF Client Readme](#git-ispf-client-readme)
    - [Purpose](#purpose)
    - [How-To Install](#how-to-install)
        - [Prerequisites](prerequisites)
        - [Copy USS files to PDS](#copy-uss-files-to-pds)
        - [Initial configuration](#initial-configuration)
     - [Running the ISPF Client](#running-the-ispf-client)
        - [Preferences](#preferences)
        - [Cloning and working with a remote repository](#cloning-and-working-with-a-remote-repository)
        - [Creating a local git repository](#creating-a-local-git-repository)

<!-- /TOC -->
------------
## Purpose

The purpose of this GitHub repository is to provide an ISPF interface that interacts with a Git repository to allow cloning, staging, checking in, pushing and pulling as well as other git commands.

------------
## How-To Install

### Prerequisites

You must have Rocket Git installed in USS on the z/OS system you plan to use. The minimum version required is 2.14.4. You can install Rocket Git using SMP/E as part of IBM z Open Development or you can go to www.rocketSoftware/git for instructions on getting Rocket Git. . You need Rocket Git to run git commands on z/OS.

### Clone GitHub project containing Git ISPF Client code

We have a chicken and egg situation. We want to use the Git ISPF client to clone repositories, but we have to clone a repository first before we can bring the Git ISPF Client code up. If you are reading this you are already at https://github.com/IBM/dbb. So you will need to do a command line clone to get the code.

* Update your .profile to have the required Rocket setup

    There are a number of variables that need to be set up and exported before you can use Git in USS. These can be set up in your .profile file, for example:

    export JAVA_HOME=/usr/lpp/java/J8.0_64
    export DBB_HOME=/var/dbbTEST/usr/lpp/IBM/dbb_1.0.5

    export GIT_SHELL=/var/rocket/bin/bash
    export GIT_EXEC_PATH=/var/rocket/libexec/git-core
    export GIT_TEMPLATE_DIR=/var/rocket/share/git-core/templates

    export PATH=$PATH:$JAVA_HOME/bin:/var/rocket/bin
    export MANPATH=$MANPATH:/var/rocket/man
    export PERL5LIB=$PERL5LIB:/var/rocket/lib/perl5

    export _BPXK_AUTOCVT=ON
    export _CEE_RUNOPTS="FILETAG(AUTOCVT,AUTOTAG) POSIX(ON)"
    export _TAG_REDIR_ERR=txt
    export _TAG_REDIR_IN=txt
    export _TAG_REDIR_OUT=txt

* Create an SSK key pair to be able to Clone

    This is covered in more detail here, https://forum.rocketsoftware.com/t/using-git-for-z-os-with-github/654, but in a nutshell do the following:

    * Logon in to Putty or OMVS
    * Issue command:
        ssh-keygen -t rsa -b 4096 -f ~/.ssh/id_rsa -C "yourid@yourdomain.com" -P ""
        Where yourid@yourdomain is the userid you use to log into the public Github.
    * Download generated key to your PC from ~/.ssh/id_rsa.pub in USS

* In Github go to Settings --> **SSH and GPG Keys** and click the **New SSH Key** button.
* Open your downloaded id_rsa.pub file and cut the contents, then paste them in the provided box in Github. Click the **Add SSH key** button.
* In Gitub in the IBM/dbb repository, open the **Clone or Download** button and click **Use SSH** and cut the SSH location, which should be **git@github.com:IBM/dbb.git**
* Go back to Putty/OMVS and clone the repo by typing the following clone command:
    git clone git@github.com:IBM/dbb.git /u/userid/Test/git1
    Where /u/userid/Test/git1 is the location where you will clone the repository to.
* The clone should work and the directory should be populated with the DBB sample code.

### Copy USS files to PDS

In the clone location there is a directory called `IDE/GitISPFClient/sbgzsamp`. There is a file there called `bgzgit.jcl`. This JCL member will allocate a set of PDS libraries and copy the files from the clone location. Make a copy of this JCL member in a data set on z/OS. Tailor as per the instuctions in the job and submit the Job. The job should finish with a return code of 0 and have populated the specified libraries.

### Initial configuration

In the libraries you created by running the BGZGIT JCL member there is a SBGZEXEC library. In there there is  BGZCONF member. Edit this member and look for the 4 install locations specified there, by default:

JAVA_HOME   = '/usr/lpp/java/J8.0_64'
Rocket_HOME = '/rsusr/ported'
DBB_HOME    = '/usr/lpp/IBM/dbb'
ICONV_HOME  = '/bin/iconv'

Change these values to where you have Java, Rocket Tools and iconv installed on your system. You can also set DBB_HOME if you have installed DBB already.

------------

## Running the ISPF Git Client

There are several methods for starting the ISPF Git client.

Start the ISPF Git client dialog using the BGZGIT REXX executable code. You can run the executable code in several ways:

* From the TSO command processor panel:
    * On the TSO command processor panel, enter **EX '<hlq>.SBGZEXEC(BGZGIT)'**
* Added to an ISPF menu:
    * Set &ZSEL to **'CMD(EX <hlq>.SBGZEXEC(BGZGIT)) NOCHECK'**. NOCHECK supports the entry of concatenated commands through the direct option (trail). On the calling panel, also specify **&ZTRAIL=.TRAIL**.
* Added as a command in the SYSPROC concatenation:
    * Create an EXEC in the SYSPROC concatenation (for example, BGZGIT) that starts the BGZGIT EXEC with any required parameters hardcoded:
    * **ex 'BGZ.SBGZEXEC(BGZGIT)'**
    * After creating the executable code, run the code from the command line. Enter the following command: **TSO %BGZGIT**.
    * If the command is added to a command table, enter **%BGZGIT**.

### Preferences

Once you have started the ISPF Git client you should go to Option **0 - Preferences**. There are 4 options here:

Preference                    | Description
------------------------------| -----------
Client Code Page              | The code page that will be used to convert between native characters on the host, and unicode escape sequences on the github repository. Default is IBM-037
Git user.email                | user.email used for Git commands - Currently not used
Git user.name                 | user name used for Git commands - Currently not used
Suppress .files from USS list | To not display .files on working directory list (default is Yes)

### Cloning a remote repository
To start working on code in a remote Git repository go to option **2 - Repository**. Enter the repository address in the line at the top of the screen, for example **git@github.com:campagboy/dbb.git**. It should be noted that Rockets Git port for z/OS currently only supports clone Git repositories through SSH. As such you will be required to set up private and public keys. In addition it should be noted that, at this time, the Git ISPF Client does not support interactively entering passwords for the keys, so as such the public/private key pair need to be created with no password. See **https://forum.rocketsoftware.com/t/using-git-for-z-os-with-github/654** for information on generating an ssh key pair on z/OS. Next to the git repository you will need to enter a working tree directory. The directory you specify will have an extra directory level added of the repository being cloned. So in the example above, if you enter **/u/myuser/git** as your working directory, then the actual working directory created will be **/u/myuser/git/dbb**. Once you press enter the repository will be cloned into the working tree directory.

It is important to note that as Git is a Hierarchical file system SCM, the folders and files in the repository are cloned into the HFS and not into PDS/PDSE.

### Working with files in a remote repository
Once a repository is cloned you can choose to work on the files either through the ISPF Git interface or you can work on them directly through ISPF UDLIST (3.17), or using your editor of choice through OMVS.

The Git commands you use generally work at a repository level as opposed to a file level. However there are some Git commands that affect individual files. Using the ISPF Git Client you can enter a **/** on any line to see a list of available options, or you can use the fast path command. On any panel press **PF1** to get a list of the available commands and their explanation. All of the repository level git commands can be entered on all the panels, for example the file list panel, but still work at the repository level. This allows you to edit a file, stage it, commit it and push it without having to return to the repository panel.

To start working with files through the ISPF Git interface enter **JU** (Jump to Working Directory) next to the repository that contains your files. From here you will see a list of folders and files. To drill down into a directory enter an **L** next to the directory. The available options at a file level are:

Option  | Description
------- | -----------
E       | Edit file (EBCDIC)
EA      | Edit file (ASCII)
EU      | Edit file (UTF-8)
V       | View file (EBCDIC)
VA      | View file (ASCII)
VU      | View file (UTF-8)
B       | Browse file
D       | Delete file
R       | Rename file
UB      | DBB User Build
UL      | View last DBB build log

To create a new file enter S <filename> on the command line. Make sure you give the filename an extension that matches an entry in your .gitattributes file. This way, when the file is added to staging it will automatically be tagged with the correct CCSID.

The User Build option works with a cloned repository that has a DBB build structure containing groovy scripts for build. The structure expected will be as follows in the z/OS Unix directory :

 **clone working directory/Build/Application/folder/filename**

To request a DBB User Build through ISPF Git interface, enter **UB** (User Build) next to the file you want to build.
On the DBB User Build panel, you will have to specify the build script to use (an existing DBB groovy build script that uses IBM Dependency Based Build), and the build destination HLQ where you want to have your file built.

### Issuing Git commands against a repository
Most Git commands work against the repository. You can enter the Git commands from any line command entry field. Again, enter a **/** to see the options available, or use the fast path Git commands which are:

Option  | Description
------- | -----------
ST      | Git Status
AD      | Git Add (or stage)
CO      | Git Commit
PS      | Git Push
CP      | Git Commit and Push
PL      | Git Pull

The above commands are the standard Git commands, but through the use of ISPF panels and tables certain actions can be performed.

#### ST - Git Status
Issues the Git status against the repository. You will be informed of files that are unstaged or uncommitted.

#### AD - Git Add
If you have modified a file, renamed a file, added a file or deleted a file, you will need to stage the change before you can commit it. When you enter the **AD** command, you will see a list of files that are currently unstaged. You can select which files, at this time, you want to add to staging.

#### CO - Git Commit
Once files are staged they can be committed. Enter the **CO** to bring up the commit dialog. At this point you will see a list of files that will be committed. You can enter an **R** next to any files you want to return to being unstaged. Then enter a comment and press enter and all the files ready to be committed will be committed.

#### PS - Git Push
Once files are committed they can be pushed to the repository. On the repository screen, if there are any files ready to be pushed you will see a "\*" next to the repository name. Enter **PS** and the files will be pushed. If there are any problems the dialog will inform you. For example if there are some chamges you need to Pull first.

#### CP - Git Commit and Push
This is a combination of commit and push in a single command.

#### PL - Git Pull
If there are any files in the repository that have been updated you will need to pull the changes into your local clone.

-------------------------
You can also enter any other valid Git command through the Git command prompt. On any line command enter a **/** and press enter, then select **Git Command Prompt**. You will be presented with a panel where you can enter any valid git command. For example Git Diff, or Git Merge.

### Adding new files
To create a new file in a directory, from the file list enter **S <filename>** and press enter. The ISPF editor panel opens where you can work on the file you have just created. Please note that to be able to successfully stage and commit the file, the extension used must exist in the **.gitattributes** file.

### Working with branches
NMext to your cloned repository enter the **BR** command to work with the branches in your repository. From here you can perform the following actions with the following commands:

Option  | Description
------- | -----------
CB      | Checkout to working branch
PB      | Push branch to origin
DB      | Delete local branch
JU      | Jump to working directory

The Checkout to local branch will replace the conmtents of your work tree with the files from the branch you selected.

### Finished working with a clone?
Once you have finished working with a clone you can remove it by entering an **RM** next to the repository name. This will delete the working directory as well.

### Working on this copy of the Git ISPF Client
If you change any of the GML and regenerate the panel members using ISPDTLC and want to store them back in the Git repository you will need to use sample job BGZOPUT to do that. This is because certain panels have chanracters that do not round trip to and from the Git repository correctly, so they need to be copied using the BINARY option. Sample job BGZOPUT.jcl is proved for that.

------------
