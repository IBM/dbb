# DBBz_Samples

This folder contains 3 examples that the IDz/ZOD team primarily uses to test their DBB functionality.  The three samples are written in ASM, PLI, and COBOL.

## Instructions to run in IDz/ZOD

All samples are ran in the same way:

1. Clone this repository into your Git Repositories tab.
2. Right-click on the "Build" folder and select "Create a z/OS Project".
3. Expand the "Build" project and the "IDE" folder.
4. Expand either ASM_Sample, COBOL_Sample, or PLI_Sample and Right-click on the file > Dependency Based Build > DBB User Build on the file that you want to do a user build on.
5. Select your z/OS system.
6. For "Select the build script to use" hit Browse and select "Build/IDE/build/build.groovy"
7. For "Enter the build sandbox folder" we recommend creating a folder named DBB_Sandbox to test the scripts.
7. Enter your build destination HLQ.
8. Entering your Team build HLQ is not needed.
9. Click the Finish button.

Your file should build and return the build results.
