# Build configuration

This directory contains the sample build configuration for the cross-application dependency sample.

`BuildPackages.yaml` is a new file with no equivalent in the shipped samples. Copy it into your build configuration directory and update the repository URL — it is ready to use as-is for this sample.

`Cobol.yaml` and `LinkEdit.yaml` are enhanced versions of the files shipped with DBB at `$DBB_HOME/samples/languages/`. If you are setting up a new build configuration, you can use these directly. If you already have these files in your build configuration directory, the sections below describe the specific additions to merge in.

## Files

| File | Based on |
|---|---|
| `BuildPackages.yaml` | New file — no shipped equivalent |
| `Cobol.yaml` | [`$DBB_HOME/samples/languages/Cobol.yaml`](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=zbuilder-getting-started) |
| `LinkEdit.yaml` | [`$DBB_HOME/samples/languages/LinkEdit.yaml`](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=zbuilder-getting-started) |

---

## `BuildPackages.yaml`

This is a new file with no shipped equivalent. It centralises the `PackageInit` and `Publish` task configuration that is common across all applications using cross-application dependencies:

- Artifact repository connection (`type`, `url`, `repositoryName`)
- `importDatasets` — the z/OS PDS datasets that `PackageInit` uploads imported binary artifacts into (`${HLQ}.IMPORTS.OBJ` for object decks, `${HLQ}.IMPORTS.BMS.COPY` for generated BMS copybooks)
- `baselineDatasets` — the z/OS PDS dataset that `PackageInit` uploads the application's own baseline build package binaries into (`${HLQ}.IMPORTS.OBJ`), so the linker finds both baseline and freshly-compiled objects in one place

Update the `url` and `repositoryName` values to match your artifact repository before use. See the main [`README.md`](../README.md) for integration steps.

---

## `Cobol.yaml`

Four additions relative to the shipped sample:

### 1. `packageSearchPath` variable

```yaml
# search path for source copybooks from imported cross-application build packages
# ${IMPORT_SRC_PATTERN} is set by PackageInit to dbb-imports/*/include/src
# when no imports are declared this variable is empty and the search returns nothing
- name: packageSearchPath
  value: search:${WORKSPACE}/?path=${IMPORT_SRC_PATTERN}/*.cpy
```

`PackageInit` sets `${IMPORT_SRC_PATTERN}` to `dbb-imports/*/include/src` after extracting import packages to the workspace. When no imports are declared, the variable is empty and this search path resolves nothing — so it is harmless for applications that have no cross-application dependencies.

### 2. `${HLQ}.IMPORTS.COPY` dataset and second `dependencyCopy` entry in `copySrc`

```yaml
# datasets:
- name: ${HLQ}.IMPORTS.COPY
  options: cyl space(1,1) lrecl(80) dsorg(PO) recfm(F,B) dsntype(library)

# copySrc step dependencyCopy:
- search: ${packageSearchPath}
  mappings:
    - source: "**/*"
      dataset: ${HLQ}.IMPORTS.COPY
```

The `packageSearchPath` search finds source copybooks staged by `PackageInit` in the workspace under `dbb-imports/*/include/src/` and copies them to `${HLQ}.IMPORTS.COPY` on z/OS, where the compiler can find them via SYSLIB. The dataset is declared unconditionally (conditional dataset creation is not currently supported by zBuilder), but will simply remain empty for applications that have no imports.

### 3. Two conditional SYSLIB entries in the `compile` step

```yaml
- {  dsn: "${HLQ}.IMPORTS.COPY",     condition: "${IMPORTS_ENABLED}", options: "shr" }
- {  dsn: "${HLQ}.IMPORTS.BMS.COPY", condition: "${IMPORTS_ENABLED}", options: "shr" }
```

These are appended to the SYSLIB concatenation after `${HLQ}.BMS.COPY`. Both are conditional on `${IMPORTS_ENABLED}`, which is set by `PackageInit` only when imports are present — applications without an `imports:` block in `dbb-app.yaml` are completely unaffected.

- `${HLQ}.IMPORTS.COPY` — source copybooks from upstream packages (staged by the `copySrc` step above)
- `${HLQ}.IMPORTS.BMS.COPY` — generated BMS map copybooks from upstream packages (uploaded directly to z/OS by `PackageInit`)

### 4. One conditional SYSLIB entry in the `linkEdit` step

```yaml
- {  dsn: "${HLQ}.IMPORTS.OBJ", condition: "${IMPORTS_ENABLED}", options: "shr" }
```

Appended to the SYSLIB concatenation after `${HLQ}.OBJ`. Conditional on `${IMPORTS_ENABLED}` for the same reason as above. `${HLQ}.IMPORTS.OBJ` is populated by `PackageInit` with the public object decks from all declared import packages.

---

## `LinkEdit.yaml`

One addition relative to the shipped sample:

### 1. One conditional SYSLIB entry in the `linkEdit` step

```yaml
- {  dsn: "${HLQ}.IMPORTS.OBJ", condition: "${IMPORTS_ENABLED}", options: "shr" }
```

Identical in purpose to the addition in `Cobol.yaml` above. The `LinkEdit` language handles `.lnk` link cards, which may also reference imported object decks by member name. This entry ensures those objects are resolvable at link time.
