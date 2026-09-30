# zBuilder Samples and Extensions

The [DBB zBuilder](https://www.ibm.com/docs/en/adffz/dbb/3.0.0?topic=building-zos-applications-zbuilder) is an integrated configuration-based build framework for building z/OS applications with DBB. Build configuration is defined in YAML files under the control of the build engineering team. The underlying implementation is comprised of DBB provided Java tasks that use the existing DBB toolkit APIs.

This directory is organized into three main areas:

## [build/](build/)

The `build/` directory contains the reference build configuration for zBuilder, including standard lifecycle configurations, language definition YAML files, and helper scripts. Build administrators can use this as a baseline for configuring application builds.

See [`build/README.md`](build/README.md) for details on setting up and customizing the build configuration.

## [extensions/](extensions/)

zBuilder provides several extension points where users can provide custom tasks and language steps to perform build operations not already available in zBuilder.

| Sample/Extension | Type | Description |
| --- | --- | --- |
| [Add Cli Args to Build Context](extensions/CliToBuildContext/) | Custom Groovy Task | This custom groovy task adds the Cli arguments with their value as variables into the build context. The variables can be used by subsequent tasks and language tasks. |
| [Check File Exists Variable](extensions/CheckFileExistsVariable/) | Language Step | This language step task extension checks for the existence of a file on z/OS Unix and then sets a boolean variable that can then be used as a `condition:` target in later language steps. |
| [File Name Base Variable](extensions/FileNameBaseVariable/) | Language Step | This language step task extension creates and sets a new language variable called `FILE_NAME_BASE` that contains just the current build file's name without file extension.<br>Example: 'MortgageApplication/cobol/epsnbrvl.cbl' --> 'epsnbrvl'. Delivered as product capability in [DBB 3.0.3](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=overview-what-is-new-noteworthy#version-303) |
| [Language Configuration](extensions/LanguageConfiguration/) | Language Step | This language step task extension allows users to provide simple YAML configuration files that can be used to override default language task configuration variables. Delivered as product capability in [DBB 3.0.3](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=overview-what-is-new-noteworthy#version-303) |
| [Process Deleted Files](extensions/ProcessDeletedFiles/) | Custom Groovy Task | This custom groovy task manages deleted files in incremental builds by adding deletion records to the DBB Build Report for packaging compatibility and deleting build artifacts. |
| [Document Build Reason](extensions/ReasonToBuildDocumentation/) | Groovy Task | This custom groovy task inspects the build list to analyze and document which files were built because they were changed or built because they are an impacted file. |
| [Add Custom Dependencies](extensions/AddCustomDependencies/) | Groovy Task | This custom Groovy task establishes _custom_ dependencies between source files and related files, such as configuration artifacts, based on matching basenames via a simple task configuration. |

## [MortgageApplication/](MortgageApplication/)

A copy of the MortgageApplication sample that ships with DBB v3.0.x provided to be easily consumable by IDz for Eclipse and IDz for VSCode IDEs to demonstrate the User Build feature.

See [`MortgageApplication/README.md`](MortgageApplication/README.md) for more details.
