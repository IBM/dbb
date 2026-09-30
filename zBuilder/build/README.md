# zBuilder Build Reference Implementation

This directory contains the canonical reference build configuration for IBM Dependency Based Build (DBB) zBuilder pipelines.

It provides a complete, working set of zBuilder YAML configurations and lifecycle definitions adapted for enterprise CI/CD environments (such as GitLab CI/CD with z/OS native runners).

---

## Overview

zBuilder relies on two levels of configuration:
1. **System / Build Framework Configuration (`$DBB_BUILD`)**: The shared language definitions, compiler/linker options, system library dataset mappings, and lifecycle definitions (contained in this directory).
2. **Application Configuration (`dbb-app.yaml`)**: Application-specific overrides, source associations, and dataset naming rules located within the application repository.

The configuration in this directory defines standard lifecycles (`file`, `full`, `impact`, `merge`, `user`, `metadata`, `release`), language tasks for mainframe technologies (COBOL, PL/I, HLASM, BMS, C/C++, REXX, LinkEdit, Transfer), and artifact packaging/publishing steps.

---

## Installation

To install this reference build framework on z/OS UNIX System Services (USS):

1. **Set `$DBB_BUILD` environment variable** in your shell profile (e.g., `~/.profile`):
   ```sh
   export DBB_BUILD=/path/to/dbb-build
   ```

2. **Copy configuration files to `$DBB_BUILD`**:
   Copy all `.yaml` files from `zBuilder/build/` to `$DBB_BUILD/`:
   ```sh
   cp zBuilder/build/*.yaml $DBB_BUILD/
   ```

3. **Install Groovy helper tasks**:
   Ensure `groovy/gitUtilsTask.groovy` is placed in `$DBB_BUILD/groovy/`:
   ```sh
   mkdir -p $DBB_BUILD/groovy
   cp zBuilder/build/groovy/gitUtilsTask.groovy $DBB_BUILD/groovy/
   ```

---

## Configuration Files

| File | Purpose |
|------|---------|
| `dbb-build.yaml` | Root build configuration defining executable lifecycles and common tasks (`Start`, `ScannerInit`, `MetadataInit`, `FileAnalysis`, `FullAnalysis`, `ImpactAnalysis`, `MergeAnalysis`, `Package`, `Publish`, `Finish`). |
| `Languages.yaml` | Aggregates all language tasks into the `Languages` stage and declares system library dataset variables (`MACLIB`, `SIGYCOMP`, `SDSNLOAD`, etc.). |
| `Cobol.yaml` | COBOL compilation and link-edit language task configuration, including Git metadata stamping and MQ stub inclusion. |
| `CobolTestcase.yaml` | COBOL Unit Test (zUnit) compile and link-edit language task configuration. |
| `Assembler.yaml` | High Level Assembler (HLASM) translation, assembly, and link-edit language task configuration. |
| `BMS.yaml` | CICS Basic Mapping Support (BMS) map copybook generation, compile, and link-edit language task configuration. |
| `PLI.yaml` | Enterprise PL/I compilation and link-edit language task configuration. |
| `CPP.yaml` | C/C++ compilation and DLL link-edit language task configuration. |
| `REXX.yaml` | REXX compilation and link-edit language task configuration. |
| `LinkEdit.yaml` | Standalone link-edit card processor task configuration. |
| `Transfer.yaml` | Direct transfer/copy language task for non-compiled assets (JCL, PROC, CNTL, REXX scripts). |
| `Db2Binds.yaml` | Db2 BIND PACKAGE and BIND PLAN generation task configuration. |
| `groovy/gitUtilsTask.groovy` | TaskScript invoked during builds to extract current Git commit hash and branch name for load module identification stamping. |

---

## Executable Lifecycles

The following lifecycles are defined in `dbb-build.yaml`:

- **`file`**: Builds a single specified program file from the command line.
- **`full`**: Performs a full build of all programs in the application workspace.
- **`impact`**: Calculates changed/impacted files since a baseline reference, builds them, and packages/publishes outputs.
- **`merge`**: Builds changed programs on a topic branch intended to be merged back into the target branch.
- **`user`**: Developer user build for a single program from an IDE, supporting error feedback, debug side files, and unit testing.
- **`metadata`**: Scans source files and existing load modules to populate dependency metadata in the DBB metadata store without rebuilding binaries.
- **`release`**: Builds impacted files for a formal release candidate, packaging outputs under the release identifier and publishing artifacts.

---

## Db2 Binds

`Db2Binds.yaml` is **intentionally excluded** from `Languages.yaml`. By default, it is configured as a user-build task conditioned on `userbuild == true`.

To enable Db2 package/plan binding in pipeline lifecycles, add `Db2Binds` explicitly to the desired lifecycle in `dbb-build.yaml` (or via `dbb-app.yaml` overrides) following the `Languages` stage.

---

## Artifact Publishing (Publish Task)

The `Publish` task in `dbb-build.yaml` packages build outputs and publishes them to an artifact manager:

- **Artifactory** (default): Configured with `type: artifactory` and the repository URL / repository name.
- **Nexus**: Supported by changing `type: nexus` and configuring the corresponding repository URL and credentials.
