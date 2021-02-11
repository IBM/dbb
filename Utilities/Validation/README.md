# Validation 

This category contains scripts that can be used to aid in the validation of product installs. 

## toolkitValidation.groovy

This script checks that the DBB toolkit API has been installed correctly on USS by using the DBB toolkit API to query product information and perform an MVSExec.

To run this script issue the following command on the command line:
```
 /usr/lpp/IBM/dbb/bin/groovyz toolkitValidation.groovy
```

The output from this script will look similar to the following:

```
Info about this DBB installation
--------------------------------
DBB Version: 1.0.4
Build Number: 32
Release Date: 05-Mar-2019 19:45:36
 
Test MVSExec of IEFBR14
-----------------------
RC: 0
```
