# zBuilder Samples and Extensions

This category contains samples and extensions that can be used with the DBB zBuilder application that was shipped in DBB 3.0 and later releases.


## Index

Sample | Description
--- | ---
| [File Exists Variable](FileExistsVariable) | This language step task extension checks for the existence of a Java file on z/OS Unix and then sets a boolean variable that can then be used as a `condition:` target in later language steps. |
| [File Name Base Variable](FileNameBaseVariable)  | This language step task extension creates and sets a new language variable called `FILE_NAME_BASE` that contains just the current build file's name without file extension.<br>Example: 'MortgageApplication/cobol/epsnbrvl.cbl' --> 'epsnbrvl'  |
| [Language Configuration](LanguageConfiguration) | This language step task extension allows users to provide simple YAML configuration files that can be used to override default language task configuration variables. 
| [Mortgage Application](MortgageApplication) | The Mortgage Application sample consists of 6 COBOL programs, 2 BMS maps, 1 link card, and 6 total copybook dependencies. It also contains a `dbb-app.yaml` file that provides application overrides specifically required to build the Mortgage Application using zBuilder. |

