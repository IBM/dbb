# README

This folder contains the environment-configurationigurations setting for each application. Application specific settings values are prefixed with `app_`.

These files are loaded during the deploy operation. It requires to pass in the **application** name as `extraVars` argument

## Purpose

The `EOLEB7-Integration.yml` file defines application-level configuration settings for the various deployment environments. These settings can override default values and are used to specify environment-specific parameters for DB2, CICS, and JCL text substitutions **for each application**. Some values may be left undefined or deleted if not applicable to the application.

## Configuration Variables

| Variable Name            | Description                                      |
|------------------------- |--------------------------------------------------|
| app_db2_plan             | DB2 plan name for the application                |
| app_db2_package          | DB2 package name for the application             |
| app_db2_qualifier        | DB2 qualifier for the application                |
| app_db2_subsys           | DB2 subsystem identifier                         |
| app_db2_sqlid            | DB2 sqlid                                        |
| app_pkg_jobcard          | DB2 package JOB card                             |
| app_plan_jobcard         | DB2 plan JOB card                                |
| app_db2_action           | DB2 action                                       |
| app_db2_sdsnload         | DB2 sdsnload                                     |
| app_db2_plan_pklist      | DB2 plan package list                            |
| app_cics_context         | CICS context for the application                 |
| app_cmci_user            | CICS CMCI user                                   |
| app_cmci_password        | CICS CMCI password                               |
| app_csd_group            | CICS CSD group                                   |
| app_cmci_scheme          | CICS schema http/https                           |
| app_cmci_host            | CICS CMCI host                                   |
| app_cmci_port            | CICS CMCI port                                   |
| app_jcl_text_subs_patterns | List of JCL text substitution patterns for JCL text substitutions, such as replacing `@PGMLIB` with the value of `{{ hlq }}`.        |

