# MortgageApplication Sample Suite

This directory contains the IBM [MortgageApplication](https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=applications-sample-mortgage-application) sample, split into five independently buildable sub-applications to demonstrate zBuilder's cross-application dependency management feature.

Each sub-application has its own `dbb-app.yaml` and can be built, versioned, and published separately. `MortgageApplication` is the consumer — it depends on build packages published by the other three provider applications.

## Sub-applications

| Sub-application | Role | Exports |
|---|---|---|
| [`NumberValidation/`](NumberValidation/) | Validates numeric input fields | Source copybook + object deck |
| [`PaymentCalculator/`](PaymentCalculator/) | Calculates monthly mortgage payments | Source copybook + object deck |
| [`MortgageMaps/`](MortgageMaps/) | BMS screen definitions | Generated BMS map copybooks |
| [`MortgageWebService/`](MortgageWebService/) | SOAP/XML converter | None |
| [`MortgageApplication/`](MortgageApplication/) | Main CICS transaction | COMMAREA copybooks |

## Dependency map

`MortgageApplication` consumes artifacts from three of the other sub-applications:

| Provider | Artifact | Type | Used for |
|---|---|---|---|
| `NumberValidation` | `epsnbrpm.cpy` | Source copybook | Compile time |
| `NumberValidation` | `EPSNBRVL.OBJ` | Object deck | Link time |
| `PaymentCalculator` | `epspdata.cpy` | Source copybook | Compile time |
| `PaymentCalculator` | `EPSMPMT.OBJ` | Object deck | Link time |
| `MortgageMaps` | `EPSMORT` map copybook | Generated BMS output | Compile time |
| `MortgageMaps` | `EPSMLIS` map copybook | Generated BMS output | Compile time |

## Build order

The three provider applications must be built and their packages published before `MortgageApplication` can be built. They have no dependencies on each other and can be built in parallel.

```
NumberValidation   ──┐
PaymentCalculator  ──┼──▶  MortgageApplication
MortgageMaps       ──┘

MortgageWebService       (no ordering constraint)
```

See each sub-application's own `README.md` for details on its contents, exported interfaces, and how to build it.
