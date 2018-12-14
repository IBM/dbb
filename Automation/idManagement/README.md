# Userid Management
The sample scripts here create and delete users on a z/OS system. There are
three files in this sample. The samples are based on RACF interfaces.

useradd - a groovy script that adds a new user to the system. It is meant to
          have a linux look and feel. In addition to providing the RACF commands
          to add a user, it adds a default dataset profile and an alias. It
          relies on a properties file that allows an installation to define a
          set of defaults for a particular type of user. It also allows the
          user to override any of the defaults on the command line.

useradd.properties - A sample flat file which contains key=value pairs for a
                     particular type of user. This file contains all of the base
                     options that the programs expect.

userdel - a groovy script that deletes a user from the system. It is meant to
          have a linux look and feel. In addition to providing the RACF commands
          to delete a user from it's database, it removes the dataset profile
          and alias.

# Notes
The groovy scripts in this folder rely on apache common cli to manage the
command line arguments. You can download the apache common cli files from:
https://commons.apache.org/proper/commons-cli/

In each of the scripts there are two hardcoded definitions that must be
modified prior to use. You will find:
a DDStatement with ("HLQ.ISPFGWY.EXEC") specification. The HLQ must be modified
to identify a High Level Qualifier on your system.
a conf directory in the statement cmd.setConfDir ("/var/dbb/conf"). The
directory needs to be created on your system (it can be any directory)

In addition the useradd script points to a properties file that has been
hardcoded to a default `/etc/IBM/` directory. This is to allow the useradd script
be run with those defaults identified.
