# NumberValidation

A sub-application of the Mortgage Application suite, built with IBM DBB zBuilder.

## Contents

| File | Description |
|---|---|
| `cobol/epsnbrvl.cbl` | Number validation sub-module (`EPSNBRVL`): validates and parses numeric input fields |
| `copybook/epsnbrpm.cpy` | `EPS-NUMBER-VALIDATION` data interface copybook |

## Build output

This application produces **no standalone load module**. The compiled object deck for `EPSNBRVL` is exported via the build package and consumed by `MortgageApplication` at link time, where it is statically linked into the `EPSCMORT` load module.

## Exported interfaces

| Type | Artifact | Consumed by |
|---|---|---|
| Source (copybook) | `epsnbrpm.cpy` | `MortgageApplication` (compile time) |
| Binary (OBJ) | `EPSNBRVL.OBJ` | `MortgageApplication` (link time) |

## Build order

This application has no upstream dependencies and can be built independently, in parallel with `PaymentCalculator`, `MortgageMaps`, and `MortgageWebService`.

## Building with DBB zBuilder

```shell
dbb build full
```
