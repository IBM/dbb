# Cross-Application Dependencies

This directory contains samples for IBM DBB zBuilder's cross-application dependency management feature. The feature lets z/OS applications publish versioned build artifacts (copybooks, object decks, generated BMS copybooks) to an artifact repository and consume them in other independently-built applications.

For a step-by-step walkthrough of setting up this feature, see the [IBM DBB documentation](#TODO-replace-with-official-doc-link).

## Overview

Large z/OS applications are often split across multiple teams, each owning a distinct component. Without a formal dependency mechanism, sharing build artifacts between those teams typically means copying files by hand, keeping everything in one shared repository, or relying on build-time conventions that are easy to break. Cross-application dependency management replaces that with an explicit, versioned contract.

Provider applications declare what they export in `dbb-app.yaml` under `application.interfaces`. When they build and publish, their exported artifacts are packaged and uploaded to an artifact repository. Consumer applications declare which upstream packages they need under `application.imports`, pinned to a specific version. At build time, zBuilder automatically downloads the required packages, makes the artifacts available to the compiler and linker, and ensures that changes to upstream packages trigger the appropriate rebuilds downstream.

Applications that have no imports are completely unaffected by this configuration.

## Samples

| Sample | Description |
|---|---|
| [`MortgageApplication/`](MortgageApplication/) | The standard MortgageApplication split into five independently buildable sub-applications, covering source copybooks, object decks, and generated BMS map copybooks as dependency types. |

## `build/` — sample build configuration

The [`build/`](build/) directory contains sample build configuration for enabling cross-application dependency support in a zBuilder build:

| File | Purpose |
|---|---|
| [`build/BuildPackages.yaml`](build/BuildPackages.yaml) | `PackageInit` and `Publish` task configuration — artifact repository connection and z/OS dataset mappings |
| [`build/Cobol.yaml`](build/Cobol.yaml) | Enhanced version of the shipped `Cobol.yaml` sample with cross-application additions |
| [`build/LinkEdit.yaml`](build/LinkEdit.yaml) | Enhanced version of the shipped `LinkEdit.yaml` sample with cross-application additions |

See [`build/README.md`](build/README.md) for a description of every change relative to the shipped samples, and how to merge these additions into an existing build configuration.

### Integration steps

1. Copy `build/BuildPackages.yaml` into your build configuration directory (alongside `dbb-build.yaml`)

2. Add it to the `include:` section of `dbb-build.yaml`:
   ```yaml
   include:
     - file: Languages.yaml
     - file: BuildPackages.yaml
   ```

3. Add `PackageInit` to the `pipeline` lifecycle in `dbb-build.yaml`, before `ImpactAnalysis`:
   ```yaml
   - lifecycle: pipeline
     tasks:
       - Start
       - ScannerInit
       - MetadataInit
       - PackageInit
       - ImpactAnalysis
       - Languages
       - Package
       - Publish
       - Finish
   ```

4. Update the `url` values in `BuildPackages.yaml` to point to your artifact repository

5. Update the `repositoryName` value under `Publish` to match your target repository

6. Merge the cross-application additions from `build/Cobol.yaml` and `build/LinkEdit.yaml` into your existing language configuration files. See [`build/README.md`](build/README.md) for the exact changes.

## Further reading

- [IBM DBB Cross-Application Dependencies — full tutorial](#TODO-replace-with-official-doc-link)
- [IBM DBB zBuilder documentation](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=zbuilder-getting-started)
- [MortgageApplication sample](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=applications-sample-mortgage-application)
