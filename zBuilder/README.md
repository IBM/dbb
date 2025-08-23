# zBuilder Samples and Extensions

The [DBB zBuilder](https://www.ibm.com/docs/en/adffz/dbb/3.0.0?topic=building-zos-applications-zbuilder) is an integrated configuration-based build framework for building z/OS applications with DBB. Build configuration is defined in YAML files under the control of the build engineering team. The underlying implementation is comprised of DBB provided Java tasks that use the existing DBB toolkit APIs. 

zBuilder provides several extension points where users can provide custom tasks and language steps to perform build operations not already available in zBuilder.


Sample/Extension | Type | Description
--- | --- | ---
| [File Exists Variable](FileExistsVariable) | Language Step | This language step task extension checks for the existence of a Java file on z/OS Unix and then sets a boolean variable that can then be used as a `condition:` target in later language steps. |
| [File Name Base Variable](FileNameBaseVariable)  | Language Step | This language step task extension creates and sets a new language variable called `FILE_NAME_BASE` that contains just the current build file's name without file extension.<br>Example: 'MortgageApplication/cobol/epsnbrvl.cbl' --> 'epsnbrvl'  |
| [Language Configuration](LanguageConfiguration) | Language Step |  This language step task extension allows users to provide simple YAML configuration files that can be used to override default language task configuration variables. 
| [Mortgage Application](MortgageApplication) | Sample |  The Mortgage Application sample consists of 6 COBOL programs, 2 BMS maps, 1 link card, and 6 total copybook dependencies. It also contains a `dbb-app.yaml` file that provides application overrides specifically required to build the Mortgage Application using zBuilder. |

