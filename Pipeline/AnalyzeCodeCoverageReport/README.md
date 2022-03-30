# Analyze Code Coverage Report

This script extracts and prints code coverage information collected by the IBM Debug Code Coverage feature.
The Code Coverage feature is typically provided in a pipeline by the Headless Code Coverage Daemon, as described in https://www.ibm.com/docs/en/developer-for-zos/15.0.0?topic=gccza-generating-code-coverage-in-headless-mode-using-daemon.

One of the artifacts that the Code Coverage Daemon generates is a ZIP file (that has the .cczip extension), that contains the collected information on code execution.
These information can be leveraged by developers as part their Unit Testing process, to understand which lines of code of their programs were executed.

The script uses the Code Coverage APIs, which is a set of Java API to parse the results contained in the CCZIP file created by the IBM Debug Code Coverage feature.
The Code Coverage APIs are described in a JAR file, that is shipped with IBM Debug (in the plugins subfolder of the IBM Debug installation on USS).
The required `ccapi.jar` file is packaged in a JAR file that must first be extracted (this JAR file is called com.ibm.debug.pdt.codecoverage.core.results_10.1.1.jar for IDz 15.0.3). Once extracted, the `ccapi.jar` must be made available (typically through to the Java classpath), to be correctly imported by the AnalyzeCodeCoverageReport script.
The script was tested on z/OS (Unix System Services).

To run the AnalyzeCodeCoverageReport script, a file or a list of comma-separated files must be provided through the `-f`/`--files` parameter. This file or these files must be CCZIP files created by the IBM Debug Code Coverage feature.
If multiple files are provided, the Code Coverage Percentage is printed for each module found, and the combined Code Coverage Percentage is printed as the "Global Code Coverage Percentage".

### Invocation example (for the Code Coverage data collected for the "DFH0XVDS" module):
``` 
 groovyz -cp /var/dbb/extensions/ccapi.jar /var/dbb/extensions/AnalyzeCodeCoverageReport.groovy -f DFH0XVDS_2022_03_29_132149_0660.cczip
```

### Output:
```
** IBM DEBUG Code Coverage details
** Included modules:
         DFH0XVDS - Code Coverage Percentage: 25
** Global Code Coverage Percentage: 25
** Build finished
```

### AnalyzeCodeCoverageReport.groovy Command Line Options
```
usage: AnalyzeCodeCoverageReport.groovy [options]
options:
 -f,--files <arg>   Comma-separated list of Code Coverage CCZIP files to
                    analyze
```
