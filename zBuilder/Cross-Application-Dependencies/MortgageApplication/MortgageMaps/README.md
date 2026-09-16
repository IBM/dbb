# MortgageMaps

A sub-application of the Mortgage Application suite, built with IBM DBB zBuilder.

This application owns the CICS BMS screen definitions for the Mortgage Application suite. It builds independently and exports the generated map copybooks as build artifacts for downstream consumption.

## Contents

| File | Description |
|---|---|
| `bms/epsmort.bms` | BMS mapset `EPSMORT` — "EPS Mortgage Calculator" data entry screen (`EPMENU` map) |
| `bms/epsmlis.bms` | BMS mapset `EPSMLIS` — "Better Mortgage Rates" lender list screen |

## Build output

The BMS compile step generates COBOL copybooks (deploy type `MAPCOPY`) from each map definition. These generated copybooks are exported via the build package and consumed by `MortgageApplication` at compile time.

## Exported interfaces

| Type | Artifact | Consumed by |
|---|---|---|
| Generated output (MAPCOPY) | `EPSMORT` copybook | `MortgageApplication` — compile time (`COPY EPSMORT` in `epscmort.cbl`) |
| Generated output (MAPCOPY) | `EPSMLIS` copybook | `MortgageApplication` — compile time (`COPY EPSMLIS` in `epsmlist.cbl`) |

## Build order

This application has no upstream dependencies and can be built independently, in parallel with `NumberValidation`, `PaymentCalculator`, and `MortgageWebService`.

## Building with DBB zBuilder

```shell
dbb build full
```
