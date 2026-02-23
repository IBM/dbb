#!/bin/sh

#
# Wazi Deploy Ansible Runtime Environment Configuration
# This file contains common settings for Wazi Deploy Ansible test scripts
#

# User Configuration
export TMPHLQ="DBEHM"

# Wazi Deploy Configuration Files
export WAZI_DEPLOY_CONFIG_FILE=/var/git/artifactoryConfig.yml

# Target Library Configuration
export TARGET_HLQ="DBEHM.WD.ANSIBLE.BASE"

# Application Configuration
export APPLICATION="base"

# Package URL
export PACKAGE_URL="http://10.3.20.231:8081/artifactory/base-dbehm-repo-local/release/rel-1.0.0/base-rel-1.0.0-2025-06-27_08-52-10.tar"

# Ansible Configuration
export ANSIBLE_INVENTORY="inventories"
export ZOS_ENVIRONMENT="int_a_zos_host"

# Ansible PATH setup
export PATH=/usr/local/bin/:$PATH

# Deployment Configuration Home
export DEPLOY_CFG_HOME="/var/git/wazi-deploy-config-sample"

echo "[INFO] - Wazi Deploy Ansible environment configured:"
echo "  - Virtual Environment: wazi-deploy-v3.0.7_preview"
echo "  - User HLQ: $TMPHLQ"
echo "  - Target HLQ: $TARGET_HLQ"
echo "  - Application: $APPLICATION"
echo "  - z/OS Environment: $ZOS_ENVIRONMENT"