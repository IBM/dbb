## Central Configuration

This directory contains the centrally managed configuration components:

* [Wazi Deploy Deployment Method](deployment-method.yml)

* Configuration of the [Wazi Deploy building block variables](global/) that are loaded during executing the deployment request via the [include_vars](https://www.ibm.com/docs/en/developer-for-zos/17.0.x?topic=blocks-include-vars) building block from the environment files config.

* [Jinja2 Templates](templates/) for generating JCL and configuration files

## Deployment Method

The `deployment-method.yml` file defines the Wazi Deploy deployment workflow for CICS applications. It orchestrates the loading of configuration, packaging, DB2/DBRM processing, deployment, rollback, JCL verification, and CICS activation steps. A key feature of this deployment method is that it processes DBRM (Database Request Module) artifacts first, copying them into a temporary dataset (configured via `type_dbrm_temp`) before deploying the main application executables. This ensures that DB2 bind operations are performed in the correct sequence and that all dependencies are handled properly.

### Configuration Loading Order

The deployment method loads configuration in the following sequence:

1. **Application Variables** - Application-specific settings from [../environment-configuration/application-overrides/](../environment-configuration/application-overrides/)
2. **Middleware Variables** - DB2 and CICS configurations from [global/](global/)
3. **Common Variables** - Type mappings and dataset specifications from [global/](global/)

This layered approach ensures that **application-level variables are loaded first**, followed by **middleware deployment configuration**, and finally **global deployment configurations and capabilities**. This allows for flexible configuration management with environment-specific and application-specific overrides taking precedence.

The order is defined in [global/global_initialization.yml](global/global_initialization.yml).

### Plan Tags Reference

Plan tags control which activities are executed during deployment:

| Tag       | Description                                                                 |
|-----------|-----------------------------------------------------------------------------|
| `always`  | Activities that run in every deployment (CONFIG, PACKAGE)                   |
| `db2`     | DB2-specific operations (DBRM processing and bind)                          |
| `deploy`  | Standard deployment operations (DEPLOY_MODULES)                             |
| `restore` | Rollback and restoration operations (ROLLBACK_MODULES, CICS activation)     |
| `never`   | Activities that only run when explicitly requested (combined with restore)  |

### Activity Overview

| Activity Name         | Description                                                                                       |
|----------------------|---------------------------------------------------------------------------------------------------|
| CONFIG               | Loads all deployment configuration variables, including application, middleware, and common/global |
| PACKAGE              | Expands the deployment package into the working directory                                          |
| DB2                  | Handles DBRM modules: deletion, update (copy to temp), and DB2 bind operations                    |
| DEPLOY_MODULES       | Deploys application artifacts (LOAD, DBRM, CICSLOAD, MAPLOAD, JCL) into target PDS libraries      |
| ROLLBACK_MODULES     | Restores artifacts from backup PDS libraries and supports DB2 bind rollback for the rollback scenario                      |
| VERIFICATION         | Verifies deployed JCLs using the JCL Expert tool                                                  |
| CICS                 | Activates CICS artifacts online via CMCI                                                          |

#### Detailed Activity Descriptions

**CONFIG**

Loads all required configuration variables for the deployment. This includes:
- Application-specific variables (environment dependent) from [../environment-configuration/](../environment-configuration/)
- Middleware variables (e.g., DB2, CICS) from [global](global/)
- Common/global variables (e.g., type mappings, dataset specs) from [global](global/)

This step ensures all subsequent activities have the necessary context and settings.

**PACKAGE**

Expands the deployment package into the working directory, preparing the artifacts for further processing and deployment.

**DB2**

This activity is dedicated to handling DBRM modules. It is always processed first, before deploying application executables. The steps include:
- Deleting obsolete DBRMs (with backup)
- Updating DBRMs by copying them into a temporary PDS library (configured via `type_dbrm_temp` in [types_pattern_mapping.yml](global/types_pattern_mapping.yml))
- Performing DB2 bind operations (package and plan) using the temporary dataset

The temporary dataset approach (`{{ hlq }}.TMP.DBRM`) ensures that:
- DB2 bind operations can access DBRMs without conflicts
- The bind process completes before final deployment
- Rollback scenarios can properly restore previous binds

This ensures that DB2-related artifacts are up-to-date and available for the application deployment.

**DEPLOY_MODULES**

Deploys application artifacts (LOAD, DBRM, CICSLOAD, MAPLOAD, JCL) into their respective target PDS libraries. Handles both updates (with backup and copy) and deletions (with backup and delete).

**ROLLBACK_MODULES**

Restores artifacts from backup PDS libraries in case a rollback is required. Also supports DB2 bind rollback operations for DBRM modules. This is dedicated to the rollback scenarios.

**VERIFICATION**

Verifies the correctness of deployed JCLs using the JCL Expert tool. This step scans and validates JCL members, ensuring compliance and quality before activation.

**CICS**

Handles the online activation of CICS artifacts (CICSLOAD, MAPLOAD) using the CMCI interface. This activity includes:
- CSD (CICS System Definition) updates via batch JCL submission
- Program updates (NEWCOPY) via CMCI for CICSLOAD and MAPLOAD modules

This step ensures that updated modules are activated in the CICS region.

## Templates

The [templates/](templates/) directory contains Jinja2 templates used for generating JCL and configuration files:

| Template                          | Description                                                    |
|-----------------------------------|----------------------------------------------------------------|
| `cics_csd_dfhcsdup.jcl.j2`       | CICS CSD update JCL template using DFHCSDUP utility            |
| `jcl_expert_validation.jcl.j2`   | JCL Expert validation template for verifying deployed JCLs     |

These templates are referenced in the configuration files and populated with environment-specific variables during deployment.

## Configuration Files

See the [global/](global/) directory for detailed information about:
- CICS configuration settings
- DB2 configuration settings
- Type pattern mappings
- PDS specifications
- JCL verification rules
- Shell command templates