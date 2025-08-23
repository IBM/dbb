# Language Configuration
This language step task extension allows users to provide simple YAML configuration files that can be used to override default language task configuration variables. With this extension users can: 
* Define an individual YAML configuration file for each program being built by a language.
* Define *Language Group* YAML configuration files for common language environments and use file patterns to assign programs to them.

## Contents
| Folder/File | Description |
| --- | --- |
| application/config/*.yaml | This folder contains the language configuration YAML files for the MortgageApplication sample.  It includes <ui><li>Individual YAML files for the MortgageApplication COBOL programs.</li></ui> <ui><li>Language group YAML files for the three types of COBOL programs.</li></ui> |
| application/LanguageGroup.yaml | This file contains `dbb-app.yaml` configuration variables when using language groups.  The sample shown is for the MortgageApplication sample.|
| groovy/LanguageConfiguration.groovy | The language configuration Groovy script that is executed when the step is run. |
| LanguageConfiguration.yaml | This file contains `dbb-build.yaml` configuration variables and step definition that can be added to any language YAML file to support language configuraition. |

## Installation Instructions
NOTE: The following installation instructions assume that that the `$DBB_BUILD` directory that was setup on z/OS Unix has been [converted into a Git repository](https://www.ibm.com/docs/en/adffz/dbb/3.0.0?topic=customization-setting-up-integrated-zbuilder-framework#convert-the-configuration-directory-to-a-git-repository-optional) and resides in a internal or cloud Git provider.

1. Clone this [DBB Community Repository](https://github.com/IBM/dbb) to your workstation.
1. Clone your zBuilder build configuration repository to your workstation.
1. Copy the `groovy/LanguageConfiguration.groovy` script to the build configuration `groovy` folder.
1. Edit the language YAML file you want to enable language configuration files.
    * Copy the variables and steps defined in the `LanguageConfiguration.yaml` file to the corresponding language YAML file sections.
    * NOTE that the LanguageConfiguration step needs to be the first step in the step list.
1. Clone your application repository on your workstation. 
1. Create a `config` directory in the application root directory.
1. Create language configuration YAML files following the YAML format shown in the sample `application/config/*.yaml` files.
    * For individual YAML file use the source file naming conventions.
    * For Language group YAML files create YAML files with descriptive names.
1. See Additional Configuration section below.
1. Commit and push your changes to your build configuration repository.
1. Pull your changes to your z/OS Unix local build configuration directory.

## Additional Configuration
The main configuration for this extension is the `languageConfiguratonSource` variable that was added to the language YAML file in the steps above:
```
- name: languageConfigurationSource
  value: ${APP_DIR}/config/${FILE_NAME}.yaml
```
By default the file pattern is configured to support individual YAML files located in the `config` directory in the application repository and using the naming convention `filename.ext.yaml`.  To support individual YAML files in an alternative location and/or using a different naming convention, modify the value accordingly.

### Language Group Configuration
An alternative to providing individual YAML configuration files is to define a *language group* YAML configuration file that can be shared by different types of programs being built by a language YAML file.  This is useful when needing to provide different complier and link-edit variable overrides for programs with EXEC CICS and EXEC SQL statements in them.  For example the compiler parms for a COBOL program with EXEC CICS and EXEC SQL would need to include `CICS,SQL` where as the compiler parms for programs with just EXEC CICS would just be `CICS`.

This scenario is easily supported by creating language group YAML files in the `config` directory:

CBL_CICS_SQL.yaml
```
config:
  - name: compileParms
    value: CICS,SQL
```    
Once the language group YAML files have been created, then it is just a simple matter of overriding the `languageConfigurationSource` variable with file associated variables pointing to the language group YAML file. Below is an example pertaining to the MortgageApplication sample.

```
- name: languageConfigurationSource
  value: ${APP_DIR}/config/CBL_CICS_SQL.yaml
  forFiles:
    - "**/cobol/epscmort.cbl"
            
- name: languageConfigurationSource
  value: ${APP_DIR}/config/CBL_CICS.yaml
  forFiles:
    - "**/cobol/epscsmrd.cbl"
    - "**/cobol/epscsmrt.cbl"
    - "**/cobol/epsmlist.cbl"
            
- name: languageConfigurationSource
  value: ${APP_DIR}/config/CBL.yaml
  forFiles:
    - "**/cobol/epsmpmt.cbl"
    - "**/cobol/epsnbrvl.cbl" 
```
This is usually done in the application's `dbb-app.yaml` file. See `application/LanguageGroup.yaml` for an example.