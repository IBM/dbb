# README

This folder contains environment files that are passed to wazideploy-deploy for the Wazi Deploy Python translators.

They contain the default values for each environment, such as default target HLQ, CICS, DB2, and z/OS Connect subsystem information.

As a design principle of this sample configuration, the environment files do not contain the full configuration of the WD Python building block variables. Instead, they introduce a set of variables that are prefixed with `default_`, which are referenced in the [global configuration files](../../deployment-configuration/global/).

The environment YAML files in this directory serve as templates for defining environment-specific default values used during deployment with Wazi Deploy. They allow deployment administrators to specify key parameters—such as dataset qualifiers, CICS, DB2, and z/OS Connect subsystem information, and other environment-dependent settings.

By using variables (often prefixed with `default_`), these files enable flexible overrides and integration with global configuration files and potential application overrides.


| Variable Name                | Description / Purpose                                                                                  | Example Value                        |
|------------------------------|--------------------------------------------------------------------------------------------------------|--------------------------------------|
| application                  | Application name                                                                                       | to be provided via extra vars        |
| hlq                          | High-level qualifier for datasets (can be overridden via extra vars, sets default for environment)     | `WDEPLOY.INT`                        |
| uss_root                     | Root directory for USS operations (can be overridden via extra vars, sets default for environment)     | `/tmp/wazi-deploy/int`               |
| deploy_cfg_home              | Path to deployment configuration home - where this repository is located (can be overridden via extra vars, sets default for environment) | `/var/wazi-deploy-config-sample/`    |
| **CICS Configuration**       |                                                                                                        |                                      |
| default_cics_user            | Default CICS user                                                                                      | `IBMUSER`                            |
| default_cics_password        | Default CICS password (the variable that contains the password)                                        | `sys1` or `default_cics_password_var`|
| default_cics_context         | Default CICS context/region                                                                            | `CICS01`                             |
| default_cmci_host            | Default CMCI (CICS Management Client Interface) hostname                                               | `127.0.0.1`                          |
| default_cmci_port            | Default CMCI (CICS Management Client Interface) port                                                   | `1490`                               |
| default_cmci_scheme          | Default CMCI (CICS Management Client Interface) scheme                                                 | `http` or `https`                    |
| default_csd_group            | Default CSD (CICS System Definition) group                                                             | `DEFAULT`                            |
| default_cics_cmci_action     | Default CICS CMCI action                                                                               | `NEWCOPY`                            |
| default_env_cics_sdfhload    | Default CICS SDFHLOAD library                                                                          | `CICSTS.V5R5.CICS.SDFHLOAD`          |
| default_env_cics_csd         | Default CICS CSD dataset                                                                               | `CICSGUSR.CICS.CICS01.DFHCSD`        |
| default_cmci_insecure        | Whether to allow insecure CMCI connections                                                             | `False`                              |
| default_cmci_cert            | Path to CMCI certificate file                                                                          | `""`                                 |
| default_cmci_key             | Path to CMCI key file                                                                                  | `""`                                 |
| **DB2 Configuration**        |                                                                                                        |                                      |
| default_db2_sdsnload         | Default DB2 SDSNLOAD library                                                                           | `DBC0CFG.DB2.V12.SDSNLOAD`           |
| default_pkg_jobcard          | Default job card for DB2 package bind jobs                                                             | `BINDPKG JOB 'WD-PKGBIND',...`       |
| default_plan_jobcard         | Default job card for DB2 plan bind jobs                                                                | `BINDPLA JOB 'WD-PLANBIND',...`      |
| default_db2_sqlid            | Default DB2 SQL ID                                                                                     | `IBMUSER`                            |
| default_db2_package          | Default DB2 package                                                                                    | `DEFAULT`                            |
| default_db2_plan             | Default DB2 plan                                                                                       | `DEFAULT`                            |
| default_db2_qualifier        | Default DB2 qualifier                                                                                  | `QUAL`                               |
| default_db2_subsys           | Default DB2 subsystem                                                                                  | `DBC1`                               |
| default_db2_plan_pklist      | Default DB2 plan package list                                                                          | `*.CBSAPK.*`                         |
| default_db2_action           | Default DB2 action                                                                                     | `REPLACE`                            |
| **z/OS Connect Configuration** |                                                                                                      |                                      |
| zos_connect_root             | Root directory for z/OS Connect server configuration                                                   | `/var/zosconnect/servers/integration/wazideploy-cics` |
| zos_connect_job_name         | Job name for z/OS Connect server                                                                       | `BAQSERV2`                           |
| zosconnect_http_port         | HTTP port for z/OS Connect server                                                                      | `9082`                               |
| **JCL Expert Configuration** |                                                                                                        |                                      |
| run_jcl_expert               | Whether to run JCL Expert validation during deployment                                                 | `False`                              |

These variables allow deployment administrators to define and override environment-specific defaults for each deployment target.