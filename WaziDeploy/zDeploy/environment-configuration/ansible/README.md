# Ansible Configuration for Wazi Deploy

This directory contains the Ansible configuration for deploying applications to z/OS environments using IBM Wazi Deploy.

## Overview

The Ansible setup provides automated deployment capabilities across multiple z/OS environments (Integration, Acceptance, Production) using the IBM Wazi Deploy Ansible collection.

## Configuration Files

### ansible.cfg

Ansible runtime configuration with optimized settings:
- **Forks**: 25 parallel executions
- **Host Key Checking**: Disabled for automated deployments
- **Stdout Callback**: Uses IBM Wazi Deploy evidence callback (`ibm.ibm_zos_wazi_deploy.cb_evidences`)
- **SSH Pipelining**: Enabled for improved performance

### deploy.yml

Main deployment playbook that:
- Targets hosts dynamically via `wd_hosts` variable (defaults to 'all')
- Controls fact gathering via `wd_gather_facts` (defaults to 'no')
- Executes deployments in serial batches (default: 5 hosts at a time)
- Imports the `ibm.ibm_zos_wazi_deploy.zos_deploy` role


## Inventory Configuration

### Environments

The inventory defines four environments:

1. **Integration (int)**: Development integration environment
   - Hosts: `int_a_zos_host`, `int_b_zos_host`
   
2. **Acceptance (accept)**: Pre-production testing environment
   - Host: `accept_zos_host`
   
3. **Production (prod)**: Production environment
   - Host: `prod_zos_host`

All hosts connect to `eoleb7.dat.ibm.com` via SSH (port 22) using the `IBMUSER` user with SSH key authentication.

### Global Variables (group_vars/all.yml)

Defines environment variables required for z/OS Python and ZOAU:

**Python & ZOAU Paths:**
- Python Virtual Environment: `/var/python-envs/wazi-deploy-v3.0.7_preview`
- Python Installation: `/usr/lpp/cyp/v3r13/pyz`
- ZOAU Installation: `/var/zoau-1.3.6.0`

**Environment Variables:**
- `_BPXK_AUTOCVT`: ON (automatic conversion)
- `PYTHONPATH`: Python site-packages path
- `LIBPATH`: ZOAU and Python library paths
- `PATH`: ZOAU and Python binary paths
- `_CEE_RUNOPTS`: Language Environment runtime options
- `LANG`: C (POSIX locale)

**Deployment Directories:**
- USS Directory: `/tmp/wd_deploy/{user}/{host}`
- Local Directory: `/tmp/wd_deploy/{user}/{host}`

**Global Configuration:**
- References: `deployment-configuration/global/global_initialization.yml`

### Host Variables (host_vars/)

Each environment host defines deployment configuration:

**Metadata:**
- API Version: `deploy.ibm.com/v1`
- Kind: Environment
- Environment name and version

**Deployment Variables:**
- `application`: Application name (overridable)
- `hlq`: High-Level Qualifier (e.g., `WDEPLOY.QA`, `WDEPLOY.INT`)
- `uss_root`: USS root directory for deployments
- `deploy_cfg_home`: Configuration home directory

and the default middleware and runtime settings, such as for Db2 and CICS.

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

## Usage

Please see sample deployment workflows outlined in the `test/ansbile` directory.

## Requirements

- Ansible 2.9 or higher
- IBM Wazi Deploy Ansible Collection (`ibm.ibm_zos_wazi_deploy`)
- SSH access to z/OS systems
Test with:
- Python 3.13 on z/OS target systems
- ZOAU 1.3.6.0 or compatible version

## Notes

- SSH pipelining is enabled for performance optimization
- All deployments use SSH key authentication
- Evidence collection is enabled via custom stdout callback
- Logging can be controlled via `wd_log` variable
- Sensitive data logging is controlled by `wd_no_log` flag