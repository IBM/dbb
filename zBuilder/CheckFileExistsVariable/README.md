# Check File Exists Variable
This language step task extension checks for the existence of a file on z/OS Unix and then sets a boolean variable that can then be used as a `condition:` target in later language steps.

Example:
```
# Check if a JCL file exists for this program          
- step: checkJCLFileExists
  type: task
  script: groovy/CheckFileExists.groovy
        
# Copy JCL file for program to dataset if exists  
- step: copyJCL
  type: copy
  condition: ${JCLFileExists}
  source: ${APP_DIR}/jcl/${FILE_NAME_BASE}.jcl
  target: //'${HLQ}.JCL(${MEMBER})'
```
## Installation Instructions
NOTE: The following installation instructions assume that that the `$DBB_BUILD` directory that was setup on z/OS Unix System Services has been [converted into a Git repository](https://www.ibm.com/docs/en/adffz/dbb/3.0.0?topic=customization-setting-up-integrated-zbuilder-framework#convert-the-configuration-directory-to-a-git-repository-optional) and resides in your Git provider of choice.

1. Clone this [DBB Community Repository](https://github.com/IBM/dbb) to your workstation.
1. Clone your zBuilder build configuration repository to your workstation.
1. Copy the `groovy/CheckFileExists.groovy` script to the build configuration `groovy` folder.
1. Edit the language YAML file you want to enable the check file exists variable.
    * Copy the variables and steps defined in the `CheckFileExists.yaml` file to the corresponding language YAML file sections.
    * NOTE that the `checkFileExists` step needs to be defined in the `steps:` list before the check file exists variable can be used in later steps.
1. See Additional Configuration section below.
1. Commit and push your changes to your build configuration repository.
1. Pull your changes to your z/OS Unix local build configuration directory.

## Additional Configuration
There are two variables for this extension that were added to the language YAML file in the steps above:
```
# The absolute path of the file to check existence.
- name: checkFileExistsPath
  value: ${APP_DIR}/jcl/${FILE_NAME_BASE}.jcl
        
# The name of the boolean variable to create and set.
- name: checkFileExistsVariableName
  value: JCLFileExists
```
The variables in the sample show how to check for a build file's corresponding JCL file.  However any file path and any variable name can be set.  Modify the values according to your needs.