# MortgageApplication

A CICS-based mortgage calculator running on IBM z/OS, built with IBM DBB zBuilder.

## Branches

| Branch | Description |
|---|---|
| `main` | Original monolithic application — all source in one repo, built as a single unit |
| `cross-app` | **This branch** — refactored for cross-application dependency management |

## What changed in this branch

This branch demonstrates IBM DBB zBuilder's **cross-application dependency management** feature. The original monolithic application has been split into five independently buildable sub-applications:

| Application | Repo | Contents | Depends on |
|---|---|---|---|
| `NumberValidation` | [github.ibm.com/Luke-Burgess/NumberValidation](https://github.ibm.com/Luke-Burgess/NumberValidation) | `EPSNBRVL`, `epsnbrpm.cpy` | — |
| `PaymentCalculator` | [github.ibm.com/Luke-Burgess/PaymentCalculator](https://github.ibm.com/Luke-Burgess/PaymentCalculator) | `EPSMPMT`, `epspdata.cpy` | — |
| `MortgageMaps` | [github.ibm.com/Luke-Burgess/MortgageMaps](https://github.ibm.com/Luke-Burgess/MortgageMaps) | BMS maps (`epsmort.bms`, `epsmlis.bms`) | — |
| `MortgageWebService` | [github.ibm.com/Luke-Burgess/MortgageWebService](https://github.ibm.com/Luke-Burgess/MortgageWebService) | `EPSCSMRD` SOAP converter | — |
| `MortgageApplication` | [github.ibm.com/Luke-Burgess/MortgageApplication](https://github.ibm.com/Luke-Burgess/MortgageApplication) | `EPSCMORT`, `EPSCSMRT`, `EPSMLIST`, link cards | `NumberValidation`, `PaymentCalculator`, `MortgageMaps` |

The following source files were moved **out** of this repo into their own sub-applications:

| File | Moved to |
|---|---|
| `cobol/epsnbrvl.cbl` | `NumberValidation` |
| `copybook/epsnbrpm.cpy` | `NumberValidation` |
| `cobol/epsmpmt.cbl` | `PaymentCalculator` |
| `copybook/epspdata.cpy` | `PaymentCalculator` |
| `bms/epsmort.bms` | `MortgageMaps` |
| `bms/epsmlis.bms` | `MortgageMaps` |
| `cobol/epscsmrd.cbl` | `MortgageWebService` |
| `application-conf/` | Removed — replaced by `dbb-app.yaml` |

## Cross-application dependencies

This application consumes three types of cross-application dependency:

| Dependency | Artifact | Dependency type | Consumed at |
|---|---|---|---|
| `NumberValidation` | `epsnbrpm.cpy` | **Source copybook** | Compile time — `COPY EPSNBRPM` in `epscmort.cbl` and `epsmlist.cbl` |
| `NumberValidation` | `EPSNBRVL.OBJ` | **Binary (object deck)** | Link time — statically linked into the `EPSCMORT` load module |
| `PaymentCalculator` | `epspdata.cpy` | **Source copybook** | Compile time — `COPY EPSPDATA` in `epscsmrt.cbl` |
| `PaymentCalculator` | `EPSMPMT.OBJ` | **Binary (object deck)** | Link time — `INCLUDE SYSLMOD(EPSMPMT)` in `link/epsmlist.lnk` |
| `MortgageMaps` | `EPSMORT` copybook | **Generated output (MAPCOPY)** | Compile time — `COPY EPSMORT` in `epscmort.cbl` |
| `MortgageMaps` | `EPSMLIS` copybook | **Generated output (MAPCOPY)** | Compile time — `COPY EPSMLIS` in `epsmlist.cbl` |

### How dependencies are resolved at build time

1. **PackageInit** runs before compilation. It downloads the declared build packages from the artifact repository, extracts them to the workspace staging area (`dbb-imports/`), and uploads non-source artifacts (object decks, generated BMS copybooks) to z/OS PDS datasets.
2. **Source copybooks** (`epsnbrpm.cpy`, `epspdata.cpy`) remain in the workspace and are found by the COBOL compiler's dependency search path — they are copied to `${HLQ}.IMPORTS.COPY` during the `copySrc` step.
3. **Object decks** (`EPSNBRVL.OBJ`, `EPSMPMT.OBJ`) are uploaded to `${HLQ}.IMPORTS.OBJ` and included in the link-edit SYSLIB.
4. **Generated BMS copybooks** (`EPSMORT`, `EPSMLIS`) are uploaded to `${HLQ}.IMPORTS.BMS.COPY` and included in the COBOL compile SYSLIB.

All dependency dataset inclusions are conditional on `${IMPORTS_ENABLED}`, which is set by `PackageInit` — builds without imports are unaffected.

## Repository contents

| File | Description |
|---|---|
| `cobol/epscmort.cbl` | Main CICS transaction controller (`EPSCMORT`, transaction ID: `EPSP`) |
| `cobol/epscsmrt.cbl` | Mortgage calculation dispatcher (`EPSCSMRT`) — bridges the COMMAREA to the payment calculator |
| `cobol/epsmlist.cbl` | Lender list display program (`EPSMLIST`) — reads VSAM file and shows matching companies |
| `copybook/epsmtcom.cpy` | CICS COMMAREA definition (wraps `epsmtinp` + `epsmtout`) |
| `copybook/epsmtinp.cpy` | COMMAREA input fields |
| `copybook/epsmtout.cpy` | COMMAREA output fields |
| `copybook/epsmortf.cpy` | VSAM mortgage company file record layout (internal only — not exported) |
| `link/epsmlist.lnk` | Link card — statically combines `EPSMPMT.OBJ` + `EPSMLIST.OBJ` into the `EPSMLIST` load module |
| `properties/epsmlist.cbl.properties` | Per-file compile option overrides for `epsmlist.cbl` |
| `crb/cics-resourcesDef.yaml` | CICS Resource Builder definitions |
| `dbb-app.yaml` | DBB zBuilder application configuration — declares imports, interfaces, and task overrides |

## Build order

`NumberValidation`, `PaymentCalculator`, and `MortgageMaps` must be built and their packages published before this application can be built. `MortgageWebService` has no ordering constraint.

```
NumberValidation   ──┐
PaymentCalculator  ──┼──▶  MortgageApplication
MortgageMaps       ──┘

MortgageWebService       (independent — no ordering constraint)
```

`NumberValidation`, `PaymentCalculator`, and `MortgageMaps` can be built in parallel.

## Building with DBB zBuilder

> **Before building:** fill in the `reference`, `buildid`, and `repository` fields in the `imports` section of `dbb-app.yaml` with values pointing to a configured artifact repository and valid published build packages from the upstream applications.

```shell
dbb build pipeline
```

The `pipeline` lifecycle runs: `PackageInit` → `ImpactAnalysis` → `Languages` → `Package`.
