#!/bin/env sh

## Test flow -
##  Run full deployment, and rollback specific artifacts

# Test script
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT_NAME=$(basename "$0")_logs

# Load Wazi Deploy configuration
. ${SCRIPT_DIR}/wazi-deploy-config.sh

# Outputs dirs
timestamp=$(date +%F_%H-%M-%S)
outputDir=${SCRIPT_DIR}/$SCRIPT_NAME/${timestamp}
evidenceDir=${outputDir}/evidences

# Folder setup
rm -Rf ${SCRIPT_DIR}/${SCRIPT_NAME}

mkdir -p $outputDir
mkdir -p $evidenceDir

#
# config - generate phase
#

deploymentMethod=${SCRIPT_DIR}/../../deployment-configuration/deployment-method.yml
configFile=$WAZI_DEPLOY_CONFIG_FILE

#
# config - deploy phase
#

# Echo version
echo "[INFO] - wazideploy-generate --version."
wazideploy-generate --version

CMD="""wazideploy-generate \
 --deploymentMethod $deploymentMethod \
 --deploymentPlan $outputDir/deploymentPlan.yaml \
 --deploymentPlanReport $outputDir/deploymentPlanReport.html \
 --packageInputFile $PACKAGE_URL \
 --packageOutputFile $outputDir/applicationArchive.tar \
 --configFile $configFile """
echo "[INFO] Executing following command : $CMD"
${CMD} | tee ${outputDir}/01-wazideploy-generate.log
rc=$?

if [ $rc -eq 0 ]; then
    echo "[INFO] - wazideploy-generate completed."
else
    echo "[WARNING] - wazideploy-generate failed."
    exit 1
fi

if [ $rc -eq 0 ]; then

    export EVIDENCES_FOLDER=$evidenceDir
    cd ../../environment-configuration/ansible/
    pwd

    ansible-playbook --version

    # Run deployment
    CMD="""ansible-playbook deploy.yml \
  -i $ANSIBLE_INVENTORY \
  -l $ZOS_ENVIRONMENT \
  -e wd_deployment_plan_file=$outputDir/deploymentPlan.yaml \
  -e wd_package_file=$outputDir/applicationArchive.tar \
  -e hlq=$TARGET_HLQ \
  -e application=$APPLICATION \
  -e deploy_cfg_home=$DEPLOY_CFG_HOME"""
    echo "[INFO] Executing following command : $CMD"
    ${CMD} | tee ${outputDir}/02-wazideploy-ansible-deploy.log
    rc=$?

    if [ $rc -eq 0 ]; then
        echo "[INFO] - ansible-playbook deploy.yml completed."
    else
        echo "[WARNING] - ansible-playbook deploy.yml failed."
        exit 1
    fi
fi

if [ $rc -eq 0 ]; then

    # Run partial rollback for specific artifact
    CMD="""ansible-playbook deploy.yml \
  -i $ANSIBLE_INVENTORY \
  -l $ZOS_ENVIRONMENT \
  -e wd_deployment_plan_file=$outputDir/deploymentPlan.yaml \
  -e wd_package_file=$outputDir/applicationArchive.tar \
  -e planTags=restore \
  -e hlq=$TARGET_HLQ \
  -e application=$APPLICATION \
  -e '{\"includes_artifacts\":[\".*BAD1.JCL\"]}' \
  -e deploy_cfg_home=$DEPLOY_CFG_HOME"""
    echo "[INFO] Executing following command : $CMD"
    ${CMD} | tee ${outputDir}/03-wazideploy-ansible-deploy-partial-rollback.log
    rc=$?

    if [ $rc -eq 0 ]; then
        echo "[INFO] - ansible-playbook deploy.yml completed."
    else
        echo "[WARNING] - ansible-playbook deploy.yml failed."
        exit 1
    fi
fi
