#!/bin/env sh

#
# Wazi Deploy Runtime Environment Configuration
# This file contains common settings for Wazi Deploy test scripts for Ansible deployment method.
#

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)

# Load Wazi Deploy configuration
. ${SCRIPT_DIR}/../wazi-deploy-test.config

# Echo WD version info
echo "[INFO] - wazideploy-generate --version."
wazideploy-generate --version

echo "[INFO] - ansible-playbook --version."
ansible-playbook --version