# zDeploy 

zDeploy is a Wazi Deploy deployment configuration framework, that allows maintaining application specific configuration along the core deployment configuration for [Wazi Deploy](https://www.ibm.com/docs/en/developer-for-zos/17.0.x?topic=deploy-whats-new-in-wazi).

## Architecture 

zDeploy maintains a single _deployment method_ for the various application architectures. The configuration is split into core configuration files, that are shared across the environments, and environment specific configuration files.

At it's core, it separates into

* a single [Wazi Deploy Deployment Method config](deployment-configuration/deployment-method.yml) implementing below [deployment scenarios](#deployment-scenarios) and **core** configuration files of the Wazi Deploy building block variables in [deployment-configuration/global/](deployment-configuration/global/)
* [environment configuration](environment-configuration/), containing the environment specific defaults such as Db2 and CICS subystem parameters, hlq config variables depending on the selected Wazi Deploy configuration path. These environment files can are passed to the wazideploy-deploy command. Variables are prefixed with `default_`, and referenced in the core configuration.
* a **centrally** managed location for any [application specific config](deployment-configuration/application-env-configurations/) overrides, that are loaded dynamically, if present. Variables are prefixed with `app_`, and allow application specific configuration.

The core configuration sets parameters for the Wazi Deploy specific building block schemas, while the environment configuration is based on simple structures. 

```log

                         ┌─────────────────────────────┐
                         │   DEPLOYMENT METHOD CORE    │
                         │  deployment-method.yml      │
                         │                             │
                         │  Activities & Plan Tags     │
                         └──────────────┬──────────────┘
                                        │
                    ┌───────────────────┴───────────────────┐
                    │                                       │
         ┌──────────▼──────────┐                 ┌──────-───▼────────┐
         │  BUILDING BLOCKS    │                 │  ENVIRONMENT      │
         │  deployment-config/ │                 │  CONFIGURATION    │
         │      /global/       │                 │  environment-     │
         │                     │                 │  configuration/   │
         │  Core Config:       │                 └─────────┬─────────┘
         │  • types_pattern_   │                           │
         │    mapping.yml      │              ┌────────────┴────────────┐
         │  • pds_spec.yml     │              │                         │
         │  • cics_config.yml  │    ┌─────────▼─────────┐    ┌──────────▼────────┐
         │  • db2_config.yml   │    │  default_* vars   │    │   app_* vars      │
         │  • jcl_verify.yml   │    │  /python/ or      │    │  /application-    │
         │  • shell_cmds.yml   │    │  /ansible/        │    │   overrides/      │
         │                     │    │                   │    │                   │
         │  Templates:         │    │  Environment      │    │  Application      │
         │  • cics_csd.jcl.j2  │    │  Defaults:        │    │  Overrides:       │
         │  • jcl_expert.jcl.j2│    │  • HLQ            │    │  • app_db2_*      │
         └─────────────────────┘    │  • USS paths      │    │  • app_cics_*     │
                                    │  • CICS subsys    │    │  • app_jcl_*      │
                                    │  • DB2 subsys     │    │  • app_jobcard    │
                                    │  • Job cards      │    │                   │
                                    └───────────────────┘    └───────────────────┘
```

## Deployment Scenarios

* **Deploy** application packages for application architectures using CICS, Db2 or Batch including
  * Processing Db2 bind operations before application binaries
  * Deploy application package to target libraries including taking backups, perform token substitutions for job control. It also takes care of processing deletions
  * Perform a JCL validation (after token substitution) based on JCL Expert 
  * CICS activation
* **Rollback** of a deployment (both full rollback and partial rollback) using the plan_tag `restore`.
  * Including Db2 rebind and CICS activation

## Prerequisites

* Tested with [Wazi Deploy 3.0.7](https://www.ibm.com/docs/en/developer-for-zos/latest?topic=deploying-zos-wazi-deploy)
* Supports both Python and Ansible deployment approaches for Wazi Deploy
* Python 3.13 on z/OS (for Python deployment method)
* ZOAU 1.3.6.3 or compatible (for Python deployment method)
* Ansible 2.9+ with IBM Wazi Deploy Ansible Collection (for Ansible deployment method)

## Setup and Install

* Take a fork of this repository
* Update environment configuration files according to your environment
* Clone the repository to IBM Unix System Services when following the Python based deployment automation and reference it in your wazideploy commands. See the [CBS](https://github.com/IBM/dbb/tree/main/Templates/Common-Backend-Scripts) for integration examples.
* Or clone it to the control node when integrating it in an Ansible based deployment automation

## Conventions and Principles

The framework follows clear naming conventions and defines an order of presedence for variable lookup.


```log
    ┌─────────────────────────────────────────────────────────────┐
    │  app_*  (Application-Level Overrides)                       │
    │  ├─ app_db2_plan, app_db2_package, app_db2_qualifier        │
    │  ├─ app_cics_context, app_cics_cmci_*, app_cics_csd_group   │
    │  └─ app_jcl_text_substitution, app_jobcard                  │
    └─────────────────────────────────────────────────────────────┘
                              ▼ Overrides
    ┌─────────────────────────────────────────────────────────────┐
    │  default_*  (Environment-Level Defaults)                    │
    │  ├─ default_hlq, default_uss_root                           │
    │  ├─ default_cics_*, default_db2_*                           │
    │  └─ default_jobcard, default_job_*                          │
    └─────────────────────────────────────────────────────────────┘
                              ▲ Uses
    ┌─────────────────────────────────────────────────────────────┐
    │  Core Building Blocks (Global Deployment Config)            │
    │  ├─ Type mappings (LOAD, DBRM, CICSLOAD, MAPLOAD, JCL)      │
    │  ├─ PDS specifications & backup strategies                  │
    │  └─ Middleware templates & verification rules               │
    └─────────────────────────────────────────────────────────────┘

```

## Testing

The repository includes comprehensive test scripts for validating deployments:

* **Python Tests** (`test/python/`): Individual test scenarios plus automated test driver
  * Run all tests: `cd test/python && ./run_all_tests.sh`
  * Run individual test: `cd test/python && ./ts1_full_deploy.sh`
  
* **Ansible Tests** (`test/ansible/`): Individual test scenarios for Ansible-based deployments
  * Run individual test: `cd test/ansible && ./ts1_full_deploy.sh`

See [test/README.md](test/README.md) for detailed test documentation and configuration instructions.
