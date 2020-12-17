# Run IBM IDZ Code Review in Batch based on the DBB Build Report

This sample groovy script let you embed the IDZ Code Review Application, also known as IDZ Software Analyzer, into your CI/CD pipeline. It requires that the IDZ code analysis tool is installed via FMID HAKGxxx. Please see the documentation at https://www.ibm.com/support/knowledgecenter/SSQ2R2_14.2.0/com.ibm.rsar.analysis.codereview.cobol.doc/topics/cac_zosbatch_overview.html

This sample groovy `RunCodeReview.groogy` script
- extracts information about the processed source code (Record Type TYPE_COPY_TO_PDS) from the DBB BuildReport.json
- assembles and runs a JCL to invoke IDZ Code Review in Batch

## Processing flow
- Read command line parameters and the mandatory ```codereview.properties``` file for externalizing parameters - either via cli parameter or from its default location.
- Read DBB's BuildReport.json from the pipeline work directory
- Parse and extract processed source code info from COPY_TO_PDS records from the build report
- Extract the SYSLIB information
- Generates an JCLExec for invoking the IDZ Code Review application
- Stores the IDZ Code review reports ```CodeReviewCSV.csv```, ```CodeReviewJUNIT.xml```  as well as the JCL spool in the workdir.

### Example invocations:
Invoke RunCodeReview.groovy passing the work directory, which stores the BuildReport.json. The property file ```codereview.properties``` is retrieved from the default location, which is the location of the RunCodeReview.groovy
```
$DBB_HOME/bin/groovyz RunCodeReview.groovy --workDir /var/dbb/buildworkspace/zAppBuild/work
```
Invoke RunCodeReview passing the work directory, which stores the BuildReport.json. The property file ```codereview.properties``` is defined via the commandline argument --properties
```
$DBB_HOME/bin/groovyz RunCodeReview.groovy --workDir /var/dbb/buildworkspace/zAppBuild/work --properties /var/dbb/integrations/codereview.properties
```

## Command Line Options Summary
```
 
  usage: RunCodeReview.groovy --workDir <path-to-dbb-buildreport> [options]
 
    options:
      -w,--workDir <dir>      Absolute path to the DBB build output directory
      -cr,--crRulesFile       (Optional) Absolute path of the rules file. If not provided, will look for it in the codereview.properties file
      -ccr, --ccrRulesFile    (Optional) Absolute path of the custom rules file. If not provided, will look for it in the codereview.properties file
      -l,--logEncoding        (Optional) Defines the Encoding for output files (JCL spool, reports),  default UTF-8
      -props,--properties     (Optional) Absolute path to the codereview.properties file
      -p,--preview            (Optional) Preview JCL, do not submit it
      -h,--help               (Optional) Prints this message
   
    requires:
  	  codeview.properties file - externalizes the JCL jobcard, RuleFile and Mappings.
  	  If --properties is not provided via cli, the script looks for it at the location of the script itself 
```

## Property file codereview.properties settings

Please review the descriptions provided for the variables within the codereview.properties file and adjust them according to your environment.