# zBuilder Samples and Extensions

This category contains samples and extensions that can be used with the DBB zBuilder application that was shipped in DBB 3.0 and later releases.


## Index

Sample | Description
--- | ---
| [File Exists Variable](FileExistsVariable) | This language step task extension checks for the existence of a Java file on z/OS Unix and then sets a boolean variable that can then be used as a `condition:` target in later language steps. |
| [File Name Base Variable](FileNameBaseVariable)  | This language step task extension creates and sets a new language variable called `FILE_NAME_BASE` that contains just the current build file's name without file extension.<br>Example: 'MortgageApplication/cobol/epsnbrvl.cbl' --> 'epsnbrvl'  |
| [Language Configuration](./LanguageConfig/README.md) | This language step task extension allows users to provide simple YAML configuration files that can be used to override default language task configuration variables. <br> With this extension users can: <ul><li>Define an individual YAML configuration file for each program being built by a language.</li><li>Define *Language Configuration Group* YAML files and use file patterns to assign programs to them.</li></ul> |
| | |

