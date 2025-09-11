# File Name Base Variable
zBuilder provides many dynamically created [language step variables](https://www.ibm.com/docs/en/adffz/dbb/3.0.0?topic=reference-predefined-variables#language-configuration-step-variables) that can be used in language YAML configuration files.  This list includes several variables containing file name elements of the current file that is being processed that are useful in language step configuration.

\* Note that this extension requires DBB v3.0.1 or later versions.

| File Variable | Description
--- | ---
| `${FILE}` | The source file that is being processed. Usually in the form of a file path relative to the ${WORKSPACE} <br>Ex. 'MortgageApplication/cobol/epsnbrvl.cbl'|
| `${FILE_NAME}` ** | The name of the source file that was processed, derived from ${FILE}. <br>Ex. 'MortgageApplication/cobol/epsnbrvl.cbl' --> 'epsnbrvl.cbl' |
| `${FILE_PATH}` ** | The absolute path of the source file. If ${FILE} is not absolute, this path will include the workspace. <br>Ex. 'MortgageApplication/cobol/epsnbrvl.cbl' --> '/u/workspace/MortgageApplication/cobol/epsnbrvl.cbl' |
| `${FILE_DIR}` ** | The absolute path to the parent directory of ${FILE}. <br>Ex. 'MortgageApplication/cobol/epsnbrvl.cbl' --> '/u/workspace/MortgageApplication/cobol' |
| `${FILE_EXT}` ** | The file extension of ${FILE}. <br>Ex. 'MortgageApplication/cobol/epsnbrvl.cbl' --> 'cbl' |
| | |

** Added in DBB v3.0.1

## Creating a `${FILE_NAME_BASE}` variable
Another useful build file name element is the file name base of a build file.  This can be useful when trying to locate or reference a file whose name is based on the build file base name.  For example, when building program `epsnbrvl.cbl` you may also need to copy a JCL file called `epsnbrvl.jcl` to a dataset as part of the build process.  As of DBB 3.0.2, there currently is not a variable that just contains the file base name element.

This step task will create a new variable for a language task that will be available to all steps that are defined after this step in the `steps:` list.

| File Variable | Description
--- | ---
| `${FILE_NAME_BASE}` | The file base name of ${FILE}. <br>Ex. 'MortgageApplication/cobol/epsnbrvl.cbl' --> 'epsnbrvl' |
| | |

## Installation Instructions
NOTE: The following installation instructions assume that that the `$DBB_BUILD` directory that was setup on z/OS Unix System Services has been [converted into a Git repository](https://www.ibm.com/docs/en/adffz/dbb/3.0.0?topic=customization-setting-up-integrated-zbuilder-framework#convert-the-configuration-directory-to-a-git-repository-optional) and resides in your Git provider of choice.

1. Clone this [DBB Community Repository](https://github.com/IBM/dbb) to your workstation.
1. Clone your zBuilder build configuration repository to your workstation.
1. Copy the `groovy/CreateFileNameBase.groovy` script to the build configuration `groovy` folder.
1. Edit the language YAML file you want to enable the `${FILE_NAME_BASE}` variable.
    * Copy the required step defined in the `FileNameBase.yaml` file to the corresponding language YAML file steps section.
    * NOTE that the `createFileNameBase` step needs to be defined in the `steps:` list before the `${FILE_NAME_BASE}` variable can be used in later steps..
1. Commit and push your changes to your build configuration repository.
1. Pull your changes to your z/OS Unix local build configuration directory.