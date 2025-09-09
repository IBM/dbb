# zBuilder Samples and Extensions

The [DBB zBuilder](https://www.ibm.com/docs/en/adffz/dbb/3.0.0?topic=building-zos-applications-zbuilder) is an integrated configuration-based build framework for building z/OS applications with DBB. Build configuration is defined in YAML files under the control of the build engineering team. The underlying implementation is comprised of DBB provided Java tasks that use the existing DBB toolkit APIs. 

zBuilder provides several extension points where users can provide custom tasks and language steps to perform build operations not already available in zBuilder.


Sample/Extension | Type | Description
--- | --- | ---
| [Check File Exists Variable](CheckFileExistsVariable) | Language Step | This language step task extension checks for the existence of a file on z/OS Unix and then sets a boolean variable that can then be used as a `condition:` target in later language steps. |
| [File Name Base Variable](FileNameBaseVariable)  | Language Step | This language step task extension creates and sets a new language variable called `FILE_NAME_BASE` that contains just the current build file's name without file extension.<br>Example: 'MortgageApplication/cobol/epsnbrvl.cbl' --> 'epsnbrvl'  |
| [Language Configuration](LanguageConfiguration) | Language Step |  This language step task extension allows users to provide simple YAML configuration files that can be used to override default language task configuration variables. |
| [Mortgage Application](MortgageApplication) | Sample | A copy of the MortgageApplication sample that ships with DBB v3.0.x provided to be easily consumable by IDz for Eclipse and IDz for VSCode IDEs to demonstate the User Build feature. |

