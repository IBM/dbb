# Global Configuration Parameters

This folder contains the global Wazi Deploy configuration files for the various Wazi Deploy building blocks used by the [deployment method](../deployment-method.yml) for the various runtime technologies.

These configuration files in this folder define settings across all targeted deployment environments.

This allows for a clear separation between building block configuration and the environment configuration.

## Configuration Override Hierarchy

The configuration system supports a flexible override mechanism using variable prefixes:

- **`default_`** prefix - Environment-level default values (defined in [../../environment-configuration/global/](../../environment-configuration/global/))
- **`app_`** prefix - Application-specific overrides (defined in [../../environment-configuration/application-overrides/](../../environment-configuration/application-overrides/))

**Precedence Order:** `app_` variables override `default_` variables

**Example:**
```yaml
# In cics_config.yml
context: "{{ app_cics_context if app_cics_context is defined and app_cics_context else default_cics_context}}"
```

This pattern allows applications to override environment defaults when needed, while maintaining consistent baseline configurations.

## Configuration Files Reference

| File Name                | Description                                                      |
|--------------------------|------------------------------------------------------------------|
| `global_initialzation.yml` | Global configuration in which the deployment configuration is loaded. Referenced by the environment configuration files.  |
| `cics_config.yml`        | Global CICS configuration settings and parameters                |
| `db2_config.yml`         | Global DB2 configuration settings and parameters                 |
| `jcl_verification.yml`   | JCL verification rules and settings                              |
| `other_defaults.yml`     | Miscellaneous default values used across deployments             |
| `pds_specification.yml`  | PDS (Partitioned Data Set) specifications and definitions        |
| `shell_commands.yml`     | Shell command definitions and reusable command templates         |
| `types_pattern_mapping.yml` | Mappings for types and pattern substitutions                  |

## Usage

These global configuration files are intended to be reused across multiple environments and deployment scenarios. Environment files can define the values overrides, based on the `default_` prefix. In various areas they accept an application override, that is identified with the `app_` prefix.

---

### File Descriptions

**pds_specification.yml**

Defines specifications and structure for Partitioned Data Sets (PDS), which are commonly used for storing source code, JCL, and other artifacts on z/OS systems. Contains three primary PDS specifications:

- **`common_pds_load_spec`** - For load modules (LIBRARY type, record format U, block size 32760)
- **`common_pds_binary_spec`** - For binary files like DBRMs (LIBRARY type, record format FB, record length 80)
- **`common_pds_txt_spec`** - For text files like JCL (LIBRARY type, record format FB, record length 80)

Additional specifications for sequential datasets (`seq_spec`, `seq_spec_a`) are also defined for specific use cases.

**types_pattern_mapping.yml**

Provides comprehensive mappings for deployment artifact types to target runtime libraries. Each type definition includes:

- **Pattern matching** - Regex patterns to identify artifact types (e.g., `.*\.LOAD$`, `.*\.DBRM$`)
- **PDS specifications** - Target and backup dataset names with allocation specs
- **Copy strategies** - Settings like `use_native_copy`, `force_lock`, `force`
- **Middleware integration** - References to CICS (`cics_systems`) and DB2 (`db2_systems`) configurations
- **USS mappings** - Directory paths for Unix System Services artifacts (BIN, SH types)

**Supported Types:**
- `LOAD` - Application load modules
- `CICSLOAD` - CICS load modules (with CICS activation)
- `MAPLOAD` - BMS map load modules (with CICS activation)
- `DBRM` - Database Request Modules (with DB2 bind)
- `JCL` - Job Control Language members
- `BIN` - Binary executables for USS
- `SH` - Shell scripts for USS

**Special Configuration:**
- `type_dbrm_temp` - Temporary DBRM dataset configuration for DB2 bind operations, using `{{ hlq }}.TMP.DBRM` as the target


#### Middleware 

**cics_config.yml**

Defines global CICS (Customer Information Control System) configuration settings and parameters. This file typically includes connection details, region names, and other CICS-specific options that are shared across environments.

**db2_config.yml**

Contains global DB2 database configuration settings, such as subsystem names, plan and package defaults, and qualifiers. These settings provide a baseline for DB2 connectivity and resource management.

#### Miscellaneous

**jcl_verification.yml**

Specifies configuration for verifying JCL (Job Control Language) scripts using JCL Expert. The configuration includes:

- **Template source** - Path to the Jinja2 template (`jcl_expert_validation.jcl.j2`)
- **Maximum return code** - Acceptable RC threshold (max_rc: 8)
- **Execution location** - Where the validation runs (LOCAL)
- **Output destination** - Generated JCL location in USS

This configuration is used by the VERIFICATION activity in the deployment method to ensure JCL quality and compliance before deployment.

**other_defaults.yml**

Holds miscellaneous default values that are used throughout the deployment process. This file is intended for parameters that do not fit into the other specific configuration categories.

**shell_commands.yml**

Lists reusable shell command templates and definitions. These commands can be referenced in deployment scripts or other configuration files to standardize execution steps. Contains sample shell scripts for demonstration and testing purposes:

- `shell_command_default` - Basic "hello world" shell command
- `procs_shell_cmd` - Sample command for procedures
- `jcl_shell_cmd` - Sample command for JCL processing

These serve as examples and can be extended for custom deployment automation needs.

## Customization Guidelines

When extending or customizing these configurations:

1. **Maintain the prefix conventions** - Use `default_` for environment defaults, `app_` for application overrides
2. **Follow the conditional pattern** - Always check if `app_` variable exists before falling back to `default_`
3. **Document new types** - Add clear descriptions for any new artifact types in `types_pattern_mapping.yml`
4. **Test PDS specifications** - Ensure allocation parameters are appropriate for your z/OS environment
5. **Validate middleware settings** - Verify CICS and DB2 configurations match your system setup

