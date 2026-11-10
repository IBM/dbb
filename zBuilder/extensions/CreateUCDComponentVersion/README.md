# CreateUCDComponentVersion Custom Task

## Overview

The `CreateUCDComponentVersion` custom Groovy task generates an UrbanCode Deploy (UCD) shiplist XML file from the DBB build report produced by the current zBuilder build, then invokes `buztool.sh` to register a new UCD component version.

This is the zBuilder-native successor to the retired `Pipeline/CreateUCDComponentVersion/dbb-ucd-packaging.groovy` standalone script. Logic has been preserved; the delivery mechanism has been converted from a CLI-driven `ScriptLoader` script to a zBuilder `TaskScript`.

## Contents

| File | Description |
| --- | --- |
| `groovy/CreateUCDComponentVersion.groovy` | Groovy task script (reads build report, generates shiplist, calls buztool). |
| `CreateUCDComponentVersion.yaml` | Task and variable configuration — include in `dbb-build.yaml`. |

## Prerequisites

- IBM DBB 3.0 or later with zBuilder.
- UrbanCode Deploy agent installed on z/OS USS with `buztool.sh` accessible.
- `buztool.sh` version that supports the `-o` output-properties option (UCD 6.2.6+).
- For UCD packaging format v2: UCD 7.2.1+ and a `containerMapping` variable.

## Installation

### 1. Copy Files

Copy both files to your `$DBB_BUILD` directory:

```
$DBB_BUILD/
├── groovy/
│   └── CreateUCDComponentVersion.groovy   ← must be in groovy/ subdirectory
└── CreateUCDComponentVersion.yaml
```

> **Note:** The Groovy script must reside in the `$DBB_BUILD/groovy` subdirectory so it is automatically discovered by the task configuration.

### 2. Configure Variables

Edit `CreateUCDComponentVersion.yaml` (or override in `dbb-app.yaml`) and set at minimum:

| Variable | Required | Description |
| --- | --- | --- |
| `UCD_BUZTOOL_PATH` | ✅ | Absolute USS path to `buztool.sh` |
| `UCD_COMPONENT` | ✅ | Name of the UCD component |
| `UCD_VERSION_NAME` | Optional | Explicit version name; omit to let UCD assign |
| `UCD_BUZTOOL_PROPERTY_FILE` | Optional | buztool property file (UCD 7.1+) |
| `UCD_ARTIFACT_REPOSITORY` | Optional | Artifact repository connection file (deprecated) |
| `UCD_V2_PACKAGE_FORMAT` | Optional | `"true"` to use UCD v2 package format |
| `UCD_CONTAINER_MAPPING` | Optional | JSON map for v2 container deployTypes |
| `UCD_PIPELINE_URL` | Optional | CI pipeline URL — stored as version property |
| `UCD_PULL_REQUEST_URL` | Optional | Pull/merge request URL — stored as version property |
| `UCD_GIT_BRANCH` | Optional | Git branch — stored as version property |
| `UCD_GIT_COMMIT_URL_PREFIX` | Optional | Git commit URL prefix for traceability |
| `UCD_GIT_TREE_URL_PREFIX` | Optional | Git tree URL prefix for source links |
| `UCD_PREVIEW` | Optional | `"true"` to generate shiplist without calling buztool |

### 3. Integrate with `dbb-build.yaml`

Include the YAML file and add the task as a post-`Languages` step:

```yaml
include:
  - file: Languages.yaml
  - file: CreateUCDComponentVersion.yaml

lifecycles:
  - lifecycle: impact
    tasks:
      - Start
      - ScannerInit
      - MetadataInit
      - ImpactAnalysis
      - Languages              # Defined in Languages.yaml
      - CreateUCDComponentVersion  # Defined in CreateUCDComponentVersion.yaml
      - Finish
```

## How It Works

1. **Read configuration** — Required (`UCD_BUZTOOL_PATH`, `UCD_COMPONENT`) and optional variables are read from the build context.
2. **Parse the build report** — Reads `BuildReport.json` from `BUILD_OUTPUT_DIR`. Collects `ExecuteRecord`, `CopyToPDSRecord`, USS/`CopyToUnixRecord`, and `DELETE_RECORD` entries. Filters out outputs with no `deployType` and `ZUNIT-TESTCASE` outputs.
3. **Deduplicate** — When a build output and a delete record both reference the same member, the entry with the higher build-report rank wins.
4. **Generate `shiplist.xml`** — Writes an XML shiplist to `$BUILD_OUTPUT_DIR/shiplist.xml`. Includes:
   - Optional CI pipeline, pull-request, and git branch properties at the version level.
   - DBB build-result URL and build properties.
   - Per-artifact git hash traceability links when `UCD_GIT_COMMIT_URL_PREFIX` and `UCD_GIT_TREE_URL_PREFIX` are set.
   - DB2 bind properties for `DBRM` outputs from `PropertiesRecord` entries.
   - Dependency inputs from `DependencySet` records.
5. **Invoke buztool.sh** — Calls `buztool.sh createzosversion` (or `createzosversion2` for v2 format). Logs output properties from `buztool.output`. Returns the buztool exit code.

If no deployable outputs are found the task exits with `0` (no-op). In preview mode the shiplist is generated but `buztool.sh` is not called.

## Example: UCD Packaging Format v2

```yaml
variables:
  - name: UCD_BUZTOOL_PATH
    value: "/var/ucd/agent/bin/buztool.sh"
  - name: UCD_COMPONENT
    value: "MortgageApplication"
  - name: UCD_BUZTOOL_PROPERTY_FILE
    value: "/u/build/conf/mortgageapp.ucd.properties"
  - name: UCD_V2_PACKAGE_FORMAT
    value: "true"
  - name: UCD_CONTAINER_MAPPING
    value: '{"LOAD":"LOAD","DBRM":"DBRM","JCL":"TEXT","COPY":"TEXT"}'
  - name: UCD_PIPELINE_URL
    value: "https://ci-server/job/MortgageApplication/42/"
  - name: UCD_GIT_BRANCH
    value: "main"
  - name: UCD_GIT_COMMIT_URL_PREFIX
    value: "https://github.com/org/mortgageapp/commit"
  - name: UCD_GIT_TREE_URL_PREFIX
    value: "https://github.com/org/mortgageapp/tree"
```
