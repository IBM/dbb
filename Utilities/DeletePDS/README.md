# DeletePDS Utility

The DeletePDS utility is used to delete PDSes on z/OS that are no longer needed.
Typically, after a build in a feature branch where datasets were created for the build, clean-up should occur to limit and optimize the required storage space on z/OS.

To delete datasets with the DeletePDS utility, you would need to specify the parameter `-h`/`--hlq` to indicate the High-Level Qualifier for your datasets to be deleted. This value is used as a filter expression for identifying the datasets to be deleted.

An other parameter `-p`/`--preview` is helpful to verify which datasets are to be deleted. Using this parameter will just display the datasets that matches the filter expression passed in the HLQ parameter of the script.


Preview:
``` 
$DBB_HOME/bin/groovyz DeletePDS.groovy -p -h BUILD.CATMAN.DEV
```
Output:
```
** Searching for all the datasets filtered with HLQ 'BUILD.CATMAN.DEV'
*** Found 'BUILD.CATMAN.DEV.ASM'
*** Found 'BUILD.CATMAN.DEV.BMS'
*** Found 'BUILD.CATMAN.DEV.BMS.COPY'
*** Found 'BUILD.CATMAN.DEV.BZU.BZUCFG'
*** Found 'BUILD.CATMAN.DEV.BZU.BZUPLAY'
*** Found 'BUILD.CATMAN.DEV.BZU.BZURPT'
*** Found 'BUILD.CATMAN.DEV.COBOL'
*** Found 'BUILD.CATMAN.DEV.COPY'
*** Found 'BUILD.CATMAN.DEV.DBRM'
*** Found 'BUILD.CATMAN.DEV.LOAD'
*** Found 'BUILD.CATMAN.DEV.MACRO'
*** Found 'BUILD.CATMAN.DEV.OBJ'
*** Found 'BUILD.CATMAN.DEV.TEST.COBOL'
*** Found 'BUILD.CATMAN.DEV.TEST.LOAD'
** Found 14 entries.
** Build finished
```

Deletion:
``` 
$DBB_HOME/bin/groovyz DeletePDS.groovy -h BUILD.CATMAN.DEV
```
Output:
```
** Deleting all datasets filtered with HLQ 'BUILD.CATMAN.DEV'
*** Deleting 'BUILD.CATMAN.DEV.ASM'
*** Deleting 'BUILD.CATMAN.DEV.BMS'
*** Deleting 'BUILD.CATMAN.DEV.BMS.COPY'
*** Deleting 'BUILD.CATMAN.DEV.BZU.BZUCFG'
*** Deleting 'BUILD.CATMAN.DEV.BZU.BZUPLAY'
*** Deleting 'BUILD.CATMAN.DEV.BZU.BZURPT'
*** Deleting 'BUILD.CATMAN.DEV.COBOL'
*** Deleting 'BUILD.CATMAN.DEV.COPY'
*** Deleting 'BUILD.CATMAN.DEV.DBRM'
*** Deleting 'BUILD.CATMAN.DEV.LOAD'
*** Deleting 'BUILD.CATMAN.DEV.MACRO'
*** Deleting 'BUILD.CATMAN.DEV.OBJ'
*** Deleting 'BUILD.CATMAN.DEV.TEST.COBOL'
*** Deleting 'BUILD.CATMAN.DEV.TEST.LOAD'
** Deleted 14 entries.
** Build finished
```



### DeletePDS.groovy Command Line Options
```
usage: DeletePDS.groovy [options]
options:
 -h,--hlq <arg>   High-Level Qualifier of datasets to delete
 -p,--preview     Only lists the datasets without actually deleting them
```
