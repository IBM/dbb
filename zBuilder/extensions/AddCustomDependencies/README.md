
# Add Custom Dependencies

## Overview
The AddCustomDependencies sample is a custom zBuilder Groovy task that establishes custom dependencies between source files and related files such as configuration artifacts. It inspects the [`SOURCE_LIST`](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=reference-task-index) build context variable and adds logical dependencies based on configurable file patterns.

This task is useful for impact build scenarios such as:
- Changes to a source file trigger processing of a related configuration file, for example to include it in a package or trigger a test case.
- Changes to a configuration file trigger processing of dependent source files.

This task assumes that the basename of the source file and dependency file is identical. For each file in the source list, the task derives the member name from the basename, searches for a matching dependency file in a configured directory, and stores the resulting logical dependency in the DBB MetadataStore.

## Contents

| Folder/File | Description |
| --- | --- |
| [groovy/addCustomDependencies.groovy](groovy/addCustomDependencies.groovy) | Groovy script that scans source files and adds matching custom logical dependencies. |

## Installation Instructions

### Copy Files
- Clone this [DBB Community Repository](https://github.com/IBM/dbb) to your workstation.
- Copy [`addCustomDependencies.groovy`](groovy/addCustomDependencies.groovy) to the `$DBB_BUILD/groovy` directory.

> **Note:** The Groovy script must reside in `$DBB_BUILD/groovy` to be discovered by the task configuration.

### Integrate with `dbb-build.yaml`

Configure one or more `AddCustomDependencies` tasks in your `dbb-build.yaml`. The task must run after analysis tasks that populate the `SOURCE_LIST` build context variable.

A practical location is in the `impact` lifecycle after `ImpactAnalysis`.

Example `dbb-build.yaml` configuration defining a bidirectional dependency between a package bind configuration file and its corresponding program:

```yaml
lifecycles:
  - lifecycle: impact
    tasks:
      - Start
      - ScannerInit
      - MetadataInit
      - ImpactAnalysis
      - AddPkgDependencyToProgram
      - AddPgmDependencyToPkgFile
      - Languages
      - Finish

tasks:
  - task: AddPkgDependencyToProgram
    script: groovy/addCustomDependencies.groovy
    variables:
      - name: dependencyfileExtension
        value: ".pkgbnd"
      - name: dependencyFilePath
        value: "config"
      - name: forFilesFilter
        value: ["**/*.cbl"]

  - task: AddPgmDependencyToPkgFile
    script: groovy/addCustomDependencies.groovy
    variables:
      - name: dependencyfileExtension
        value: ".cbl"
      - name: dependencyFilePath
        value: "cobol"
      - name: forFilesFilter
        value: ["**/*.pkgbnd"]
```

> **Note:** Task variables can also be configured or overridden in `dbb-app.yaml`.

## How It Works
The [`addCustomDependencies.groovy`](groovy/addCustomDependencies.groovy) script performs the following:

1. Reads the `SOURCE_LIST` build context variable populated by analysis tasks such as `ImpactAnalysis`, `FullAnalysis`, or `MergeAnalysis`.
2. Derives the basename of each file in `SOURCE_LIST`.
3. Searches the workspace for a matching file under the configured [`dependencyFilePath`](#required-task-variables).
4. Retrieves the source file's `LogicalFile` object from the MetadataStore.
5. Adds a custom `LogicalDependency` to the source file.

## Required Task Variables

The script supports the following task configuration variables:

| Variable | Description |
| --- | --- |
| `dependencyfileExtension` | File extension of the dependency file to match |
| `dependencyFilePath` | Path to dependency files relative to `APP_DIR` |
| `forFilesFilter` | List of glob patterns for files that should receive custom dependencies |

## Console Output
When the task runs, it reports:
- each custom dependency that was added
- how many matching dependency files were found
- how many custom dependencies were added

In verbose mode, it also reports changed files that did not have a matching dependency file.

## Task Dependencies
This task consumes the following build context variable:

- `SOURCE_LIST`: set of files changed in the current build, typically populated by analysis tasks such as `ImpactAnalysis`

This task must run after analysis tasks in zBuilder because they identify files during incremental build processing.

- `MetadataInit` must run before this task to establish a DBB MetadataStore connection

This task is not intended for lifecycles that do not use a DBB MetadataStore connection, such as the `user` lifecycle.

## Complete Working Example

### Repository Structure
```text
myapp/
├── cobol/
│   ├── FILEA.cbl
│   ├── FILEB.cbl
│   └── FILEC.cbl
└── config/
    ├── FILEA.pkgbnd
    ├── FILEB.pkgbnd
    └── FILEC.pkgbnd
```

### Configuration in `dbb-build.yaml`
```yaml
lifecycles:
  - lifecycle: impact
    tasks:
      - Start
      - ScannerInit
      - MetadataInit
      - ImpactAnalysis
      - AddPkgDependencyToProgram
      - AddPgmDependencyToPkgFile
      - Languages
      - Finish

tasks:
  - task: AddPkgDependencyToProgram
    script: groovy/addCustomDependencies.groovy
    variables:
      - name: dependencyfileExtension
        value: ".pkgbnd"
      - name: dependencyFilePath
        value: "config"
      - name: forFilesFilter
        value: ["**/*.cbl"]
  
  # Adds the COBOL program as a dependency to the package file
  - task: AddPgmDependencyToPkgFile
    script: groovy/addCustomDependencies.groovy
    variables:
      - name: dependencyfileExtension
        value: ".cbl"
      - name: dependencyFilePath
        value: "cobol"
      - name: forFilesFilter
        value: ["**/*.pkgbnd"]
```

### Scenario Execution

**Scenario A: Developer changes `FILEA.cbl`**
1. `ImpactAnalysis` adds `cobol/FILEA.cbl` to `SOURCE_LIST`
2. `AddPkgDependencyToProgram` finds `config/FILEA.pkgbnd`
3. Dependency added: `FILEA.cbl` → `FILEA.pkgbnd`
4. Subsequent build processing adds `FILEA.pkgbnd` to the build list when the COBOL program is modified. This is useful when the package bind file must also be processed and packaged.

**Scenario B: Developer changes `FILEB.pkgbnd`**
1. `ImpactAnalysis` adds `config/FILEB.pkgbnd` to `SOURCE_LIST`
2. `AddPgmDependencyToPkgFile` finds `cobol/FILEB.cbl`
3. Dependency added: `FILEB.pkgbnd` → `FILEB.cbl`
4. The build recompiles the COBOL program to generate a new DBRM for rebinding when the package bind file is modified.

## Implementation Notes

- **Member Name Derivation**: The member name is extracted from the file basename by removing the file extension. For example, `cobol/FILEA.cbl` becomes `FILEA`.

- **Dependency Path**: The dependency target is the repository-relative path of the matching dependency file (e.g., `config/FILEA.pkgbnd`).

- **File Matching**: The task searches for exactly one matching file per source file. The file must exist at `{APP_DIR}/{dependencyFilePath}/{memberName}{dependencyfileExtension}`.

- **Dependency Category**: The dependency category is derived from `dependencyfileExtension` by removing any leading dot and converting the value to uppercase.

- **Glob Pattern Matching**: The `forFilesFilter` uses standard glob patterns:
  - `**/*.cbl` - matches all `.cbl` files in any directory
  - `**/config/*.pkgbnd` - matches `.pkgbnd` files only in `config` directories
  - `["**/*.cbl", "**/*.CBL"]` - matches both lowercase and uppercase extensions