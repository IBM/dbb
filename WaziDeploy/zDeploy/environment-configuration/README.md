# Environment Configuration

This directory contains environment-specific configuration files and settings for Wazi Deploy operations. It organizes deployment configurations across different environments (Integration, Acceptance, Production) and provides mechanisms for both global defaults and application-specific overrides.

## Directory Structure

### `/global`

Contains environment-specific default configuration files that define baseline settings for each deployment environment.

**Purpose:**
- Define default values for environment-specific parameters (HLQs, CICS subsystems, DB2 configurations, etc.)
- Serve as templates for deployment administrators to specify key parameters
- Use variables prefixed with `default_` that are referenced in the [deployment-configuration/global](../deployment-configuration/global/) files
- Enable flexible overrides and integration with application-specific configurations

**Sample Files:**
- `EOLEB7-Global.yml` - Global settings shared across all environments
- `EOLEB7-Integration.yml` - Integration environment defaults
- `EOLEB7-Acceptance.yml` - Acceptance environment defaults
- `EOLEB7-Production.yml` - Production environment defaults

**Common Variables:**
- Dataset qualifiers (HLQ)
- USS root directories
- CICS configuration (user, context, CMCI settings)
- DB2 configuration (subsystem, packages, plans, qualifiers)
- Job cards and other environment-dependent settings

See [global/README.md](./global/README.md) for detailed variable descriptions.

### `/application-overrides`

Contains application-specific configuration overrides organized by customization level.

**Purpose:**
- Override default environment settings for specific applications
- Provide application-level customization of DB2, CICS, and JCL parameters
- Support different customization strategies through base and customer subdirectories

**Subdirectories:**

#### `/application-overrides/base`
Base-level application overrides that apply to standard application configurations.

#### `/application-overrides/customer`
Customer-specific application overrides that extend or replace base configurations for customized deployments.

**Configuration Files:**
Each subdirectory contains environment-specific files:
- `EOLEB7-Integration.yml`
- `EOLEB7-Acceptance.yml`
- `EOLEB7-Production.yml`

**Variables:**
Application settings are prefixed with `app_` and include:
- DB2 configuration (plan, package, qualifier, subsystem, sqlid)
- CICS configuration (context, CMCI settings, CSD group)
- JCL text substitution patterns
- Job cards for DB2 operations

See [application-overrides/README.md](./application-overrides/README.md) for detailed variable descriptions.

### `/ansible`

Contains Ansible-specific configuration and playbooks for executing Wazi Deploy operations.

**Purpose:**
- Configure Ansible runtime behavior for deployment operations
- Define the main deployment playbook that orchestrates the deployment process
- Manage inventory and host group variables

**Key Files:**
- `ansible.cfg` - Ansible configuration settings (forks, callbacks, SSH options)
- `deploy.yml` - Main deployment playbook that imports the `ibm.ibm_zos_wazi_deploy.zos_deploy` role
- `inventories/inventory.yml` - Ansible inventory defining target hosts
- `inventories/group_vars/all.yml` - Variables applied to all hosts in the inventory

**Features:**
- Configurable parallelism and host management
- Evidence collection callbacks for deployment tracking
- SSH connection optimization
- Integration with IBM Wazi Deploy Ansible collection

## Configuration Hierarchy

The configuration follows a hierarchical override pattern:

1. **Global Defaults** (`/global`) - Base environment settings
2. **Application Base Overrides** (`/application-overrides/base`) - Standard application-specific settings
3. **Application Customer Overrides** (`/application-overrides/customer`) - Customer-specific customizations

Variables are resolved in order, with later configurations overriding earlier ones.

## Usage

These configuration files are loaded during deployment operations and require:
- The **application** name to be passed as an `extraVars` argument
- The **environment** to be specified (Integration, Acceptance, or Production)
- Proper Ansible inventory configuration in the `/ansible` directory

## Variable Naming Conventions

- `default_*` - Environment-level default values (defined in `/global`)
- `app_*` - Application-specific overrides (defined in `/application-overrides`)
- Standard variables - Direct configuration values (e.g., `hlq`, `uss_root`, `application`)

## Related Documentation

- [Deployment Configuration](../deployment-configuration/README.md) - Global deployment rules and templates
- [Global Environment Variables](./global/README.md) - Detailed environment variable reference
- [Application Overrides](./application-overrides/README.md) - Application-specific configuration reference