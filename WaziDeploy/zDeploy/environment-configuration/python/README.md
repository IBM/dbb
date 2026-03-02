# README 

This folder contains environment files that are passed to wazideploy-deploy for the Wazi Deploy Python translators.

They contain the default values for each environment, such as, default target HLQ, CICS or DB2 subsystem information.

As a design principle of this sample configuration, the environment files do not contain the full configuration of the WD Python building block variables. Instead it introduces a set of variables that are prefixed with `default_`, that are referenced in the [global configuration files](../../deployment-configuration/global/) 

The environment YAML files in this directory serve as templates for defining environment-specific default values used during deployment with Wazi Deploy. They allow deployment administrators to specify key parameters—such as dataset qualifiers, CICS and DB2 subsystem information, and other environment-dependent settings.

By using variables (often prefixed with `default_`), these files enable flexible overrides and integration with global configuration files and potential application overrides.


| Variable Name            | Description / Purpose                                                                                  | Example Value     |
|------------------------- |------------------------------------------------------------------------------------------------------|----------------------------------|
| application              | Application name                                                                                     | to be provided via extra vars    |
| hlq                      | High-level qualifier for datasets (can be overridden via extra vars, sets default for environment)   | `WDEPLOY.INT`                    |
| uss_root                 | Root directory for USS operations (can be overridden via extra vars, sets default for environment)   | `/tmp/wazi-deploy`               |
| deploy_cfg_home          | Path to deployment configuration home - where this repository is located (can be overridden via extra vars, sets default for environment) | `/var/wazi-deploy-config-sample/`  |
| default_cics_user        | Default CICS user                                                                                    | `IBMUSER`                         |
| default_cics_password    | Default CICS password var   (the variable that contains the password)                                | `default_cics_password_var`       |
| default_cics_context     | Default CICS context/region                                                                          | `CICS01`                          |
| default_cmci_host        | Default CMCI (CICS Management Client Interface) hostname                                             | `127.0.0.1`                       |
| default_cmci_port        | Default CMCI (CICS Management Client Interface) port                                                 | `1490`                            |
| default_cmci_scheme      | Default CMCI (CICS Management Client Interface) scheme                                               | `https`                           |
| default_csd_group        | Default CMCI (CICS Management Client Interface) group                                                | `DEFAULT`                         |
| default_cics_cmci_action | Default CICS CMCI action                                                                             | `NEWCOPY`                         |
| default_db2_sdsnload     | Default DB2 SDSNLOAD library                                                                         | `DBC0CFG.DB2.V12.SDSNLOAD`        |
| default_pkg_jobcard      | Default job card for DB2 package bind jobs                                                           | `BINDPKG JOB ...`                 |
| default_plan_jobcard     | Default job card for DB2 plan bind jobs                                                              | `BINDPLA JOB ...`                 |
| default_db2_sqlid        | Default DB2 sqlid                                                                                    | `IBMUSER`                         |
| default_db2_package      | Default DB2 package                                                                                  | `DEFAULT`                         |
| default_db2_plan         | Default DB2 plan                                                                                     | `DEFAULT`                         |
| default_db2_qualifier    | Default DB2 qualifier                                                                                | `QUAL`                            |
| default_db2_subsys       | Default DB2 sub-system                                                                               | `DBC1`                            |
| default_db2_plan_pklist  | Default DB2 plan packge list                                                                         | `*.CBSAPK.*`                      |
| default_db2_action       | Default DB2 action                                                                                   | `REPLACE`                         |

These variables allow deployment administrators to define and override environment-specific defaults for each deployment target.