# PaymentCalculator

A sub-application of the Mortgage Application suite, built with IBM DBB zBuilder.

## Contents

| File | Description |
|---|---|
| `cobol/epsmpmt.cbl` | Mortgage payment calculator sub-module (`EPSMPMT`): computes monthly payment amount from principal, rate, and term |
| `copybook/epspdata.cpy` | `EPSPDATA` data interface copybook: input/output layout for the payment calculation |

## Build output

This application produces **no standalone load module**. The compiled object deck for `EPSMPMT` is exported via the build package and consumed by `MortgageApplication` at link time, where it is statically linked into the composite `EPSMLIST` load module via `link/epsmlist.lnk`.

## Exported interfaces

| Type | Artifact | Consumed by |
|---|---|---|
| Source (copybook) | `epspdata.cpy` | `MortgageApplication` (compile time, used in `epscsmrt.cbl`) |
| Binary (OBJ) | `EPSMPMT.OBJ` | `MortgageApplication` (link time, `INCLUDE SYSLMOD(EPSMPMT)` in `epsmlist.lnk`) |

## Build order

This application has no upstream dependencies and can be built independently, in parallel with `NumberValidation` and `MortgageWebService`.

```
NumberValidation   ──┐
                      ├──▶  MortgageApplication
PaymentCalculator  ──┘
```

## Building with DBB zBuilder

```shell
dbb build pipeline
```
