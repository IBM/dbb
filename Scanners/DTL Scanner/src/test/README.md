# Test the DTL Scanner

## Pre-requisites 

You need to build the scanner and upload them to z/OS Unix System services

## Run test

* Upload the supplied sample dtl source [FLMB#P.dtlenu](dtl/FLMB#P.dtlenu) to a location in USS.
* Upload the supplied sample groovy script to USS
* Customize paths in [dependencyScanner.groovy](groovy/dependencyScanner.groovy) to the locations where you uploaded the sample source codes.
* Execute 

```
IBMUSER:/u/ibmuser: >/usr/lpp/dbb/v2r0/bin/groovyz -cp /var/dbb/dbb-extensions/dtlScanner_0.1.0.jar /u/ibmuser/userBuild/dependencyScanner.groovy
{
  "lname": "FLMB#P",
  "file": "FLMB#P.dtlenu",
  "language": "DTL",
  "cics": false,
  "sql": false,
  "dli": false,
  "mq": false,
  "logicalDependencies": [
    {
      "lname": "ISPFMENU",
      "library": "DTLINC",
      "category": "COPY"
    },
    {
      "lname": "ISPFSCLM",
      "library": "DTLINC",
      "category": "COPY"
    },
    {
      "lname": "ISPDUTIL",
      "library": "DTLINC",
      "category": "COPY"
    },
    {
      "lname": "ISPFJOBC",
      "library": "DTLINC",
      "category": "COPY"
    },
    ....
```