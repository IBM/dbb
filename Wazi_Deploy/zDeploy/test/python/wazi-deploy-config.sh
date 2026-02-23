#!/bin/env sh

#
# Wazi Deploy Runtime Environment Configuration
# This file contains common settings for Wazi Deploy test scripts
#

# User Configuration
export TMPHLQ="DBEHM"

# Python Virtual Environment Setup
echo "[INFO] - Setup Python virtual environment."
unset PYTHONPATH
. /var/python-envs/wazi-deploy-v3.0.7_preview/bin/activate

# ZOAU (Z Open Automation Utilities) Configuration
export ZOAU_HOME=/var/zoau-1.3.6.0
export PATH=$ZOAU_HOME/bin:$PATH
export LIBPATH=$ZOAU_HOME/lib:$LIBPATH

# Wazi Deploy Configuration Files
export WAZI_DEPLOY_CONFIG_FILE=/var/WaziDeploy/config/WaziDeploy-ConfigFile.yml

# Target Library Configuration
export TARGET_HLQ="DBEHM.WD.PYTHON.BASE"

# Application Configuration
export APPLICATION="base"

# Package URL
export PACKAGE_URL="http://10.3.20.231:8081/artifactory/base-dbehm-repo-local/release/rel-1.0.0/base-rel-1.0.0-2025-06-27_08-52-10.tar"

echo "[INFO] - Wazi Deploy environment configured:"
echo "  - Virtual Environment: wazi-deploy-v3.0.7_preview"
echo "  - ZOAU Home: $ZOAU_HOME"
echo "  - User HLQ: $TMPHLQ"
echo "  - Target HLQ: $TARGET_HLQ"
echo "  - Application: $APPLICATION"