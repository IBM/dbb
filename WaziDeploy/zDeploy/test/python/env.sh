#!/bin/env sh

#
# Wazi Deploy Runtime Environment Configuration
# This file contains common settings for Wazi Deploy test scripts
#

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)

# Load Wazi Deploy configuration
. ${SCRIPT_DIR}/../wazi-deploy-test.config

# ZOAU (Z Open Automation Utilities) Configuration
export ZOAU_HOME=$ZOAU_HOME_DIR
export PATH=$ZOAU_HOME/bin:$PATH
export LIBPATH=$ZOAU_HOME/lib:$LIBPATH

# Python Virtual Environment Setup
echo "[INFO] - Setup Python virtual environment."
unset PYTHONPATH
. $PYTHON_VENV/bin/activate

# Echo WD version info
echo "[INFO] - wazideploy-generate --version."
wazideploy-generate --version