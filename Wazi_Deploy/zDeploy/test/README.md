# Test Scripts

This directory contains test scripts for validating Wazi Deploy configurations using both Ansible and Python deployment methods.

## Test Cases

| Test Case | Script Name | Description |
|-----------|-------------|-------------|
| **TS1** | `ts1_full_deploy.sh` | **Full Deployment** - Executes a complete deployment of the application package. Generates the deployment plan and deploys all artifacts to the target z/OS environment. |
| **TS2** | `ts2_deploy_tags.sh` | **Tagged Deployment** - Demonstrates selective deployment using tags. First deploys only DB2-related artifacts (using `db2` tag), then deploys remaining artifacts (skipping `db2` tag). Useful for staged deployments. |
| **TS3** | `ts3_full_rollback.sh` | **Full Rollback** - Performs a complete deployment followed by a full rollback of the entire package. Uses the `restore` tag to revert all deployed artifacts to their previous state. |
| **TS4** | `ts4_partial_rollback.sh` | **Partial Rollback** - Executes a full deployment followed by a selective rollback of specific artifacts. Demonstrates rolling back individual components (e.g., `.*BAD1.JCL`) while leaving other deployed artifacts intact. |

## Configuration

Before running the test scripts, you must configure the `wazi-deploy-config.sh` file in either the `ansible/` or `python/` directory. This configuration file serves as a template for deploying your own application packages.

### Configuration Settings

The `wazi-deploy-config.sh` file contains the following key settings:

| Setting | Description | Example |
|---------|-------------|---------|
| `TMPHLQ` | Temporary high-level qualifier for z/OS datasets | `DBEHM` |
| `ZOAU_HOME` | Path to Z Open Automation Utilities installation | `/var/zoau-1.3.6.0` |
| `WAZI_DEPLOY_CONFIG_FILE` | Path to Wazi Deploy configuration file (Artifactory/repository settings) | `/var/git/artifactoryConfig.yml` |
| `TARGET_HLQ` | Target high-level qualifier for deployed datasets | `DBEHM.WD.ANSIBLE.BASE` |
| `APPLICATION` | Application name identifier | `base` |
| `PACKAGE_URL` | URL or path to the application package (tar file) | `http://server:8081/artifactory/.../package.tar` |
| `ANSIBLE_INVENTORY` | Ansible inventory directory (Ansible only) | `inventories` |
| `ZOS_ENVIRONMENT` | Target z/OS environment host (Ansible only) | `int_a_zos_host` |
| `DEPLOY_CFG_HOME` | Path to deployment configuration repository (Ansible only) | `/var/git/wazi-deploy-config-sample` |

### Customizing for Your Application

To use these test scripts with your own application package:

1. Update the following required settings:
   - `TMPHLQ`: Your z/OS user ID or temporary HLQ
   - `TARGET_HLQ`: Target library prefix for your application
   - `APPLICATION`: Your application name
   - `PACKAGE_URL`: Path or URL to your application package
1. Verify environment paths:
   - Python virtual environment path (line 14)
   - `ZOAU_HOME` path
   - `WAZI_DEPLOY_CONFIG_FILE` path
1. For Ansible deployments, also configure:
   - `ZOS_ENVIRONMENT`: Target host from your inventory
   - `DEPLOY_CFG_HOME`: Path to your deployment configuration repository

**Note**: The configuration file is sourced by all test scripts, ensuring consistent settings across test cases.

## Usage

Both Ansible and Python implementations follow the same test case structure. Each test script:

1. Loads configuration from `wazi-deploy-config.sh`
2. Generates a deployment plan using `wazideploy-generate`
3. Executes deployment operations using either:
   - **Ansible**: `ansible-playbook deploy.yml`
   - **Python**: `wazideploy-deploy`
4. Creates timestamped log directories with deployment evidence

### Running Individual Tests

To run a single test:

```bash
# Ansible version
cd test/ansible
./ts1_full_deploy.sh

# Python version
cd test/python
./ts1_full_deploy.sh
```

### Running All Tests (Python Only)

The Python test directory includes a test driver that runs all test scenarios and reports results:

```bash
cd test/python
./run_all_tests.sh
```

**Test Driver Features:**
- Runs all 4 test scenarios sequentially (ts1-ts4)
- Tracks pass/fail status for each test
- Generates timestamped results directory with individual test logs
- Creates summary report showing total/passed/failed counts
- Returns exit code 0 if all tests pass, 1 if any fail
- Results saved to: `test/python/test_results/YYYY-MM-DD_HH-MM-SS/`
