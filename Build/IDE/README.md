# DBB Samples for IDz/ZOD

This folder contains 3 examples that the IDz/ZOD IDE team primarily uses to test their DBB functionality.  The three samples are written in ASM, PLI, and COBOL.  These can be used as a simple test within IDz or ZOD to ensure your DBB feature is functioning correctly.

## Instructions to run in IDz/ZOD

All samples are ran in the same way:

1. Clone this repository into your Git Repositories tab.  Ensure you are connected to a server running DBB.
2. Right-click on the "Build" folder and select "Create a z/OS Project".
3. Expand the "Build" project and the "IDE" folder.
4. Expand the "build" folder and open the "datasets.properties" file.
5. This file must be configured based on your system.  For more information, see the below section.
6. Expand either ASM_Sample, COBOL_Sample, or PLI_Sample and Right-click on the file > Dependency Based Build > DBB User Build on the file that you want to do a user build on.
7. Select your z/OS system.
8. For "Select the build script to use" hit Browse and select "Build/IDE/build/build.groovy"
9. For "Enter the build sandbox folder" we recommend creating a folder named DBB_Sandbox to test the scripts.
10. Enter your build destination HLQ.
11. Entering your Team build HLQ is not needed.
12. Click the Finish button.

Your file should build and return the build results.

## Configuring the datasets.properties file

The datasets.properties file is the only file you will have to alter to fit your system.  The file contains build properties for Partition Data Sets (PDS) used by the build scripts.  Within the file, we have included the PDS we use as examples.
