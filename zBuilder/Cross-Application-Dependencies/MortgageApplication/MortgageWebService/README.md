# MortgageWebService

A sub-application of the Mortgage Application suite, built with IBM DBB zBuilder.

## Contents

| File | Description |
|---|---|
| `cobol/epscsmrd.cbl` | SOAP converter driver (`EPSCSMRD`) and four nested sub-programs: `EPSCSMRF` (LE exception handler), `EPSCSMRX` (conversion metadata), `EPSCSMRI` (XML→language structure), `EPSCSMRO` (language structure→XML) |

## Purpose

`EPSCSMRD` is a CICS web services pipeline converter. It is invoked by the CICS web services infrastructure (not by the interactive CICS transaction) and handles XML/SOAP conversion for the mortgage calculation service.

All five programs are defined in a single source file and compile together as one unit.

## Build output

Produces a standalone `EPSCSMRD` CICS load module. No other application consumes this application's outputs.

## Build order

This application has no upstream dependencies and no downstream consumers. It can be built in any order, fully independently of `NumberValidation`, `PaymentCalculator`, and `MortgageApplication`.

## Building with DBB zBuilder

```shell
dbb build pipeline
```
