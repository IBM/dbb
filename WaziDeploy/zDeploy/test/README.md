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

Before running the test scripts, you must configure the [wazi-deploy-test.config](wazi-deploy-test.config) file in the test directory. This centralized configuration file is used by both Ansible and Python test scripts.

### Configuration Settings

The [wazi-deploy-test.config](wazi-deploy-test.config) file contains the following key settings:

| Setting | Description | Example |
|---------|-------------|---------|
| `TMPHLQ` | Temporary high-level qualifier for z/OS datasets | `DBEHM` |
| `DEPLOYMENT_METHOD` | Path to deployment method YAML file | `${SCRIPT_DIR}/../../deployment-configuration/deployment-method.yml` |
| `DEPLOYMENT_CONFIG_HOME` | Base directory for deployment configuration | `${SCRIPT_DIR}/../../` |
| `TARGET_HLQ` | Target high-level qualifier for deployed datasets | `DBEHM.WD.ANSIBLE.BASE` |
| `APPLICATION` | Application name identifier | `base` |
| `PACKAGE_URL` | URL or path to the application package (tar file) | `http://10.3.20.231:8081/artifactory/.../package.tar` |
| `ANSIBLE_INVENTORY` | Ansible inventory directory | `inventories` |
| `ZOS_ENVIRONMENT` | Target z/OS environment host | `int_a_zos_host` |
| `WAZI_DEPLOY_CONFIG_FILE_ANSIBLE` | Wazi Deploy config file for Ansible (Artifactory/repository settings) | `/var/git/artifactoryConfig.yml` |
| `PYTHON_VENV` | Python virtual environment path | `/var/python-envs/wazi-deploy-v3.0.7.1` |
| `ZOAU_HOME_DIR` | Path to Z Open Automation Utilities installation | `/var/zoau-1.3.6.0` |
| `WAZI_DEPLOY_CONFIG_FILE_PYTHON` | Wazi Deploy config file for Python (Artifactory/repository settings) | `/var/WaziDeploy/config/WaziDeploy-ConfigFile.yml` |

### Customizing for Your Application

To use these test scripts with your own application package:

1. Edit [wazi-deploy-test.config](wazi-deploy-test.config) and update the following required settings:
   - `TMPHLQ`: Your z/OS user ID or temporary HLQ
   - `TARGET_HLQ`: Target library prefix for your application
   - `APPLICATION`: Your application name
   - `PACKAGE_URL`: Path or URL to your application package
2. Verify environment paths:
   - `PYTHON_VENV`: Python virtual environment path
   - `ZOAU_HOME_DIR`: Z Open Automation Utilities path
   - `WAZI_DEPLOY_CONFIG_FILE_ANSIBLE`: Ansible config file path
   - `WAZI_DEPLOY_CONFIG_FILE_PYTHON`: Python config file path
3. For Ansible deployments, also configure:
   - `ANSIBLE_INVENTORY`: Your inventory directory
   - `ZOS_ENVIRONMENT`: Target host from your inventory

**Note**: The centralized configuration file is sourced by both [ansible/env.sh](ansible/env.sh) and [python/env.sh](python/env.sh), ensuring consistent settings across all test cases.

## Usage

Both Ansible and Python implementations follow the same test case structure. Each test script:

1. Loads configuration from [wazi-deploy-test.config](wazi-deploy-test.config) via the env.sh.
2. Generates a deployment plan using wazideploy-generate
3. Executes deployment operations using either:
   - **Ansible**: ansible-playbook deploy.yml
   - **Python**: wazideploy-deploy
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
- Runs all 4 test scenarios sequentially ([ts1](python/ts1_full_deploy.sh)-[ts4](python/ts4_partial_rollback.sh))
- Tracks pass/fail status for each test
- Generates timestamped results directory with individual test logs
- Creates summary report ([test_results.log](python/test_results/)) showing total/passed/failed counts
- Returns exit code 0 if all tests pass, 1 if any fail
- Results saved to: `test/python/test_results/YYYY-MM-DD_HH-MM-SS/`

## Environment Setup

### Ansible Environment

The [ansible/env.sh](ansible/env.sh) script:
- Sources the centralized [wazi-deploy-test.config](wazi-deploy-test.config)
- Sets up PATH for Ansible
- Displays version information for `wazideploy-generate` and `ansible-playbook`

### Python Environment

The [python/env.sh](python/env.sh) script:
- Sources the centralized [wazi-deploy-test.config](wazi-deploy-test.config)
- Configures ZOAU (Z Open Automation Utilities) environment variables
- Activates the Python virtual environment
- Displays version information for `wazideploy-generate`

## Test Output

Each test creates a timestamped directory structure:

```
test/
├── ansible/
│   └── ts1_full_deploy.sh_logs/
│       └── YYYY-MM-DD_HH-MM-SS/
│           ├── 01-wazideploy-generate.log
│           ├── 02-wazideploy-ansible-deploy.log
│           ├── deploymentPlan.yaml
│           ├── deploymentPlanReport.html
│           ├── applicationArchive.tar
│           └── evidences/
│               └── (deployment evidence files)
└── python/
    ├── ts1_full_deploy.sh_logs/
    │   └── YYYY-MM-DD_HH-MM-SS/
    │       ├── 01-wazideploy-generate.log
    │       ├── 02-wazideploy-deloy.log
    │       ├── deploymentPlan.yaml
    │       ├── deploymentPlanReport.html
    │       ├── applicationArchive.tar
    │       └── evidences/
    │           └── evidence.yaml
    └── test_results/
        └── YYYY-MM-DD_HH-MM-SS/
            ├── test_results.log
            ├── ts1_full_deploy.sh.log
            ├── ts2_deploy_tags.sh.log
            ├── ts3_full_rollback.sh.log
            └── ts4_partial_rollback.sh.log
```
