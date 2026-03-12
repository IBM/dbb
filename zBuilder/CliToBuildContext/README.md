# CliToBuildContext Custom Task

## Overview

The CliToBuildContext custom Groovy task reads the command line arguments passed to the zBuilder framework and adds them as [variables](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=reference-predefined-variables) into the Build Context.

## Contents
| Folder/File | Description |
| --- | --- |
| groovy/CliToBuildContext.groovy | Groovy script that implements the logic. |
| CliToBuildContext.yaml | YAML configuration file defining the task that can be added to dbb-build.yaml . |

## Installation Instructions

### Copy Files

- Clone this [DBB Community Repository](https://github.com/IBM/dbb) to your workstation.
- Copy the `CliToBuildContext.groovy` script to the `$DBB_BUILD/groovy` directory.
- Update dbb-build.yaml to include the new task .

> **Note:** The Groovy script must reside in the `$DBB_BUILD/groovy` subdirectory to be automatically discovered by the task configuration.

### Integrate task in dbb-build.yaml

- Include the `CliToBuildContext.yaml` in your `dbb-build.yaml` to source the task and variable configuration.
- Add the `CliToBuildContext` task to the lifecycles where you want the cli arguments be available as variables in the build context.

Example `dbb-build.yaml` configuration:
```yaml
include: 
  - file: Languages.yaml
  - file: CliToBuildContext.yaml

# sample lifecycle
lifecycles:
  - lifecycle: impact
    tasks:
      - Start
      - ScannerInit
      - CliToBuildContext # Defined in the include
      - MetadataInit
      - ImpactAnalysis
      - Languages    # Defined in Languages.yaml
      - Finish
```

## How It Works

The `CliToBuildContext.groovy` script performs the following:

1. **Read cli command object**: Reads the cli object and starts to loop over the arguments.
2. **Add key value pairs into the build workspace**: For cli arguments with options, the value of the option is added to the build context. For cli argument without options, the name of key and the value `true` is added. 
3. **Reference cli arguments**: Subsequent tasks can reference the cli option as a variable. Be aware that this is case-sensitive.
