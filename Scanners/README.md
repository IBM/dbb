# Scanners

This category contains examples how to build your custom dependency scanner based on DBBs' `AbstractDependencyScanner` API.
Samples are supposed to follow the same structure. The custom dependency scanner may either be a scanner implementation by itself or extend an existing scanner.

## Building a scanner with IDZ

* Clone the IBM/DBB repository into IDZ/Eclipse.
* Import the scanner implementation as a JAVA project
* Download the `<DBB-HOME>/lib/dbb.core_2.0.x.x.jar` package from your IBM DBB toolkit installation on z/OS Unix System Services to your development environment.
* Fix the setup of the build path of the scanner project by adding the DBB core package as an External JAR under the Libraries configuration and remove any invalid references.
* Implement changes in `runScan()` method of the scanner implementation that is extending `AbstractDependencyScanner`.
* Create a `Run Configuration` for a Java Application for the scanner implementation (Note: The Run Configuration will not have a main method as we will not actually be using it to run the program. You might see some warnings.)
* Execute the `Export Java > Runnable JAR file` action in IDZ/Eclipse, which will create the JAR file containing your custom scanner:
  * Select the previously created runner configuration
  * Select the export destination
  * In the Library handling section, select to `Copy required libraries into a sub-folder next to the generated JAR` option
* Copy the generated JAR to z/OS Unix system services as binary
* Run the provided test script which each scanner implementation should provide. Details are included in the scanner project.
 
## Scanner implementations

The below list contains the available scanner implementations

Sample | Description
--- | ---
[DTL Scanner](DTL%20Scanner) | A custom scanner implementation for z/OS ISPF Dialog Tag Language to detect its dependencies. zAppBuild contains the language script for DTL. The scanner implementation leverages regex patterns to identify the referenced include files.
