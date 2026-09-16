# MortgageApplication

The consumer application in the cross-application dependency sample. This is the main CICS-based mortgage calculator, refactored from the original [MortgageApplication](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=applications-sample-mortgage-application) monolith. It depends on build packages published by `NumberValidation`, `PaymentCalculator`, and `MortgageMaps`.

## Cross-application dependencies

This application consumes three types of cross-application dependency:

| Dependency | Artifact | Dependency type | Consumed at |
|---|---|---|---|
| `NumberValidation` | `epsnbrpm.cpy` | **Source copybook** | Compile time (`COPY EPSNBRPM` in `epscmort.cbl` and `epsmlist.cbl`) |
| `NumberValidation` | `EPSNBRVL.OBJ` | **Binary (object deck)** | Link time (statically linked into the `EPSCMORT` load module) |
| `PaymentCalculator` | `epspdata.cpy` | **Source copybook** | Compile time (`COPY EPSPDATA` in `epscsmrt.cbl`) |
| `PaymentCalculator` | `EPSMPMT.OBJ` | **Binary (object deck)** | Link time (`INCLUDE SYSLMOD(EPSMPMT)` in `link/epsmlist.lnk`) |
| `MortgageMaps` | `EPSMORT` copybook | **Generated output (MAPCOPY)** | Compile time (`COPY EPSMORT` in `epscmort.cbl`) |
| `MortgageMaps` | `EPSMLIS` copybook | **Generated output (MAPCOPY)** | Compile time (`COPY EPSMLIS` in `epsmlist.cbl`) |

## Repository contents

| File | Description |
|---|---|
| `cobol/epscmort.cbl` | Main CICS transaction controller (`EPSCMORT`, transaction ID: `EPSP`) |
| `cobol/epscsmrt.cbl` | Mortgage calculation dispatcher (`EPSCSMRT`): bridges the COMMAREA to the payment calculator |
| `cobol/epsmlist.cbl` | Lender list display program (`EPSMLIST`): reads VSAM file and shows matching companies |
| `copybook/epsmtcom.cpy` | CICS COMMAREA definition (wraps `epsmtinp` + `epsmtout`) |
| `copybook/epsmtinp.cpy` | COMMAREA input fields |
| `copybook/epsmtout.cpy` | COMMAREA output fields |
| `copybook/epsmortf.cpy` | VSAM mortgage company file record layout (internal only, not exported) |
| `link/epsmlist.lnk` | Link card: statically combines `EPSMPMT.OBJ` + `EPSMLIST.OBJ` into the `EPSMLIST` load module |
| `properties/epsmlist.cbl.properties` | Per-file compile option overrides for `epsmlist.cbl` |
| `crb/cics-resourcesDef.yaml` | CICS Resource Builder definitions |
| `dbb-app.yaml` | DBB zBuilder application configuration: declares imports, interfaces, and task overrides |
## Build order

`NumberValidation`, `PaymentCalculator`, and `MortgageMaps` must be built and their packages published before this application can be built. See [`../README.md`](../README.md) for the full build order across the suite.


## Building with DBB zBuilder

> **Before building:** fill in the `reference`, `buildId`, and `repository` fields in the `imports` section of `dbb-app.yaml` with values pointing to a configured artifact repository and valid published build packages from the upstream applications.

```shell
dbb build pipeline
```

The `pipeline` lifecycle runs: `PackageInit` → `ImpactAnalysis` → `Languages` → `Package`.
