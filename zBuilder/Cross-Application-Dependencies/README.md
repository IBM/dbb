# Cross-Application Dependencies

This directory contains samples for IBM DBB zBuilder's **cross-application dependency management** feature, which enables z/OS applications to publish and consume versioned build artifacts — copybooks, object decks, and generated BMS copybooks — across independently buildable application repositories.

For a complete step-by-step walkthrough of setting up cross-application dependencies, see the [IBM DBB documentation](#TODO-replace-with-official-doc-link).

## Overview

In a traditional monolithic DBB build, all source files live in one repository and are compiled together. As applications grow, teams often need to split them into independently owned components: one team owns a shared calculation module, another owns the maps, another owns the main transaction. Cross-application dependency management makes this practical by:

- Letting **provider** applications publish versioned **build packages** containing their exported copybooks and object decks to an artifact repository (Artifactory, Nexus)
- Letting **consumer** applications declare those packages as `imports` in `dbb-app.yaml` and have them automatically downloaded, staged, and made available to the compiler and linker at build time
- Integrating with **ImpactAnalysis** so changes to an upstream package's artifacts trigger the appropriate rebuilds in downstream consumers

The feature is additive — applications with no `imports` declaration are completely unaffected by this configuration.

## MortgageApplication Sample

The `MortgageApplication/` subdirectory contains IBM's standard [MortgageApplication](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=applications-sample-mortgage-application) sample, refactored from a single monolith into five independently buildable sub-applications. Each sub-application is a self-contained zBuilder application with its own `dbb-app.yaml`.

### Sub-applications

| Sub-application | Contents | Exports | Depends on |
|---|---|---|---|
| `NumberValidation/` | `EPSNBRVL` sub-module + `epsnbrpm.cpy` | copybook (source) + OBJ (binary) | — |
| `PaymentCalculator/` | `EPSMPMT` sub-module + `epspdata.cpy` | copybook (source) + OBJ (binary) | — |
| `MortgageMaps/` | BMS mapsets (`epsmort.bms`, `epsmlis.bms`) | MAPCOPY generated copybooks | — |
| `MortgageWebService/` | SOAP converter (`EPSCSMRD`) | nothing | — |
| `MortgageApplication/` | Main CICS transaction (`EPSCMORT`, `EPSCSMRT`, `EPSMLIST`) | COMMAREA copybooks (source) | `NumberValidation`, `PaymentCalculator`, `MortgageMaps` |

### Dependency types exercised

The sample covers all three cross-application dependency types supported by zBuilder:

| Type | Provider | Artifact | Consumer usage |
|---|---|---|---|
| **Source copybook** | `NumberValidation` | `epsnbrpm.cpy` | `COPY EPSNBRPM` in `epscmort.cbl`, `epsmlist.cbl` — compile time |
| **Source copybook** | `PaymentCalculator` | `epspdata.cpy` | `COPY EPSPDATA` in `epscsmrt.cbl` — compile time |
| **Object deck (OBJ)** | `NumberValidation` | `EPSNBRVL.OBJ` | Statically linked into `EPSCMORT` — link time |
| **Object deck (OBJ)** | `PaymentCalculator` | `EPSMPMT.OBJ` | `INCLUDE SYSLMOD(EPSMPMT)` in `epsmlist.lnk` — link time |
| **Generated BMS copybook (MAPCOPY)** | `MortgageMaps` | `EPSMORT` copybook | `COPY EPSMORT` in `epscmort.cbl` — compile time |
| **Generated BMS copybook (MAPCOPY)** | `MortgageMaps` | `EPSMLIS` copybook | `COPY EPSMLIS` in `epsmlist.cbl` — compile time |

### Build order

`NumberValidation`, `PaymentCalculator`, and `MortgageMaps` must be built and their packages published before `MortgageApplication` can be built. `MortgageWebService` has no ordering constraint.

```
NumberValidation   ──┐
PaymentCalculator  ──┼──▶  MortgageApplication
MortgageMaps       ──┘

MortgageWebService       (independent — no ordering constraint)
```

The three Tier 1 applications can be built in parallel.

## How it works

### 1. Provider applications declare `interfaces`

Each provider application declares what it exports in its `dbb-app.yaml` under `application.interfaces`. Source files (copybooks) go under `sources`; compiled outputs (object decks, generated BMS copybooks) go under `outputs` with a `usage: public` flag:

```yaml
# NumberValidation/dbb-app.yaml (excerpt)
application:
  interfaces:
    sources:
      - "${APP_DIR_NAME}/copybook/*.cpy"   # exports epsnbrpm.cpy
    outputs:
      - description: "EPSNBRVL object deck"
        usage: public
        deployTypes:
          - OBJ
        buildFilePatterns:
          - "${APP_DIR_NAME}/cobol/epsnbrvl.cbl"
```

When the provider builds with `dbb build pipeline`, the `Package` task assembles a build package TAR from these declarations and `Publish` uploads it to the artifact repository.

### 2. Consumer applications declare `imports`

The consumer declares which upstream packages it needs in `application.imports`, pinned to a specific `reference` and `buildId`:

```yaml
# MortgageApplication/dbb-app.yaml (excerpt)
application:
  imports:
    - name: NumberValidation
      type: "release"
      reference: "1.0.0"
      buildId: "2025-07-01_10-00-00"
      repository: dbb-zbuilder
```

### 3. `PackageInit` stages dependencies before compilation

When the consumer builds with `dbb build pipeline`, `PackageInit` runs before `ImpactAnalysis` and the language tasks:

- Downloads any missing build packages from the artifact repository to a local persistent cache
- Extracts source artifacts (copybooks) to `${WORKSPACE}/dbb-imports/<app-name>/include/src/`
- Uploads binary artifacts (OBJ, MAPCOPY) to z/OS PDS datasets (`${HLQ}.IMPORTS.OBJ`, `${HLQ}.IMPORTS.BMS.COPY`)
- Sets `${IMPORTS_ENABLED}=true` in the build context

### 4. Language tasks consume staged artifacts transparently

The `Cobol.yaml` build configuration includes a `packageSearchPath` that resolves source copybooks from the staging area and copies them to `${HLQ}.IMPORTS.COPY`. The SYSLIB concatenation for both the compile and link-edit steps conditionally includes the import datasets when `${IMPORTS_ENABLED}` is set — so applications without imports are completely unaffected.

## `BuildPackages.yaml` — shared build configuration

`BuildPackages.yaml` in this directory is a sample shared build configuration file that must be included in your `dbb-build.yaml` alongside your language configurations. It centralises the `PackageInit` and `Publish` task settings that are common across all applications:

- Artifact repository URL and type
- Import dataset mappings (`importDatasets`) — where PackageInit uploads OBJ and MAPCOPY artifacts from upstream packages
- Baseline dataset mappings (`baselineDatasets`) — where PackageInit uploads artifacts from the application's own previous build package for incremental builds

### Integration steps

1. Copy `BuildPackages.yaml` into your build configuration directory (alongside `dbb-build.yaml`)

2. Add it to the `include:` section of `dbb-build.yaml`:
   ```yaml
   include:
     - file: Languages.yaml
     - file: BuildPackages.yaml   # add this line
   ```

3. Add `PackageInit` to the `pipeline` lifecycle in `dbb-build.yaml`, before `ImpactAnalysis`:
   ```yaml
   - lifecycle: pipeline
     tasks:
       - Start
       - ScannerInit
       - MetadataInit
       - PackageInit        # add this line
       - ImpactAnalysis
       - Languages
       - Package
       - Publish
       - Finish
   ```

4. Update the `url` values in `BuildPackages.yaml` to point to your artifact repository instance

5. Update the `repositoryName` value under `Publish` to match your target repository

6. Ensure `Cobol.yaml` (and `LinkEdit.yaml`) include the dependency datasets in SYSLIB with conditional inclusion on `${IMPORTS_ENABLED}`:
   ```yaml
   # In Cobol.yaml compile step dds:
   - { name: "SYSLIB", dsn: "${HLQ}.COPY", options: "shr" }
   - {                 dsn: "${HLQ}.BMS.COPY", options: "shr" }
   - {                 dsn: "${HLQ}.IMPORTS.COPY",     condition: "${IMPORTS_ENABLED}", options: "shr" }
   - {                 dsn: "${HLQ}.IMPORTS.BMS.COPY", condition: "${IMPORTS_ENABLED}", options: "shr" }

   # In LinkEdit.yaml link-edit step dds:
   - { name: "SYSLIB", dsn: "${HLQ}.OBJ", options: "shr" }
   - {                 dsn: "${HLQ}.IMPORTS.OBJ", condition: "${IMPORTS_ENABLED}", options: "shr" }
   ```

   A `packageSearchPath` variable and corresponding `dependencyCopy` mapping are also required in `Cobol.yaml` to stage source copybooks from the workspace into `${HLQ}.IMPORTS.COPY`. See the [IBM DBB documentation](#TODO-replace-with-official-doc-link) for the full language configuration.

## Further reading

- [IBM DBB Cross-Application Dependencies — full tutorial](#TODO-replace-with-official-doc-link)
- [IBM DBB zBuilder documentation](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=zbuilder-getting-started)
- [MortgageApplication sample](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=applications-sample-mortgage-application)
