# ProcessDeletedFiles Custom Task

## Overview
The ProcessDeletedFiles custom Groovy task manages deleted files in zBuilder incremental builds by adding deletion records to the build report and removing leftover build artifacts. It relies on context provided by the `ImpactAnalysis` task and is designed to run during incremental build lifecycles. Deletion records in the build report can be used by deployment tools to remove deployed artifacts in later stages of the pipeline.

## Contents
| Folder/File | Description |
| --- | --- |
| groovy/ProcessDeletedFiles.groovy | Groovy script that implements the logic for processing deleted files. |
| ProcessDeletedFiles.yaml | YAML configuration file defining the task and its associated variables. |

## Installation Instructions

### 1. Copy Files

- Clone this [DBB Community Repository](https://github.com/IBM/dbb) to your workstation.
- Copy the `ProcessDeletedFiles.groovy` script to the `$DBB_BUILD/groovy` directory.
- Copy the `ProcessDeletedFiles.yaml` file to your `$DBB_BUILD` directory.

> **Note:** The Groovy script must reside in the `$DBB_BUILD/groovy` subdirectory to be automatically discovered by the task configuration.

---

### 2. Configure Output Libraries

The `outputLibs` variable in the YAML file maps file patterns (e.g., `*.cbl`, `*.pli`, `*.asm`) to the corresponding output datasets (e.g., `${HLQ}.OBJ`, `${HLQ}.LOAD`). Review and adjust these mappings to match your environment.

You can also move these variable definitions to the application level by defining in `dbb-app.yaml` if preferred.

---

### 3. Integrate with dbb-build.yaml

- Include the `ProcessDeletedFiles.yaml` in your `dbb-build.yaml` to source the task and variable configuration.
- Add the `ProcessDeletedFiles` task to the `impact` lifecycle in `dbb-build.yaml`. It should execute directly after the `ImpactAnalysis` task. 

Example `dbb-build.yaml` configuration:
```yaml
include: 
  - file: Languages.yaml
  - file: ProcessDeletedFiles.yaml

lifecycles:
  - lifecycle: impact
    tasks:
      - Start
      - ScannerInit
      - MetadataInit
      - ImpactAnalysis
      - ProcessDeletedFiles  # Defined in ProcessDeletedFiles.yaml
      - Languages    # Defined in Languages.yaml
      - Finish
```

---

## How It Works

The `ProcessDeletedFiles.groovy` script performs the following:

1. **Identifies Deleted Files**: Reads the list of deleted files from the build context. This list is populated by the `ImpactAnalysis` task. 
2. **Generates Build Report Records**: Creates a `DELETE_RECORD` for each deleted file.
3. **Deletes Output Members**: Removes existing corresponding members from the output datasets using JZOS APIs.

If no deleted files are found, the task exits gracefully with a return code of `0`.

---

## Example Configuration Snippet

```yaml
tasks:
  - task: ProcessDeletedFiles
    variables:
      - name: outputLibs
        forFiles: "**.cbl"
        value:
          - "${HLQ}.OBJ"
          - "${HLQ}.DBRM"
          - "${HLQ}.LOAD"
```

---

## Task Dependencies

This task consumes the DELETED_FILES context variable which is populated by the `ImpactAnalysis` task. Therefore it must run after the ImpactAnalysis task executes. 

## Additional Notes

- The task is compatible with DBB's build report system and is designed to integrate into the `impact` lifecycle, as it requires the `ImpactAnalysis` task to execute. 
- You can define multiple `outputLibs` entries using `forFiles` to support various file types like `.bms`, `.pli`, `.asm`, `.rexx`, etc.
