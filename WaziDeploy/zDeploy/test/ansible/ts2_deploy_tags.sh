#!/bin/env sh

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

echo "[INFO] - Wazi Deploy Ansible environment configured:"
echo "  - User HLQ: $TMPHLQ"
echo "  - Target HLQ: $TARGET_HLQ"
echo "  - Application: $APPLICATION"
echo "  - z/OS Environment: $ZOS_ENVIRONMENT"

#
# config - generate phase
#

deploymentMethod=${SCRIPT_DIR}/../../deployment-configuration/deployment-method.yml
configFile=$WAZI_DEPLOY_CONFIG_FILE
deploy_config_home=${SCRIPT_DIR}/../../

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
fi

if [ $rc -eq 0 ]; then

    echo "[INFO] - ansible-playbook deploy using db2 tag."

    export EVIDENCES_FOLDER=$evidenceDir
    cd ../../environment-configuration/ansible/
    pwd

    ansible-playbook --version

    CMD="""ansible-playbook deploy.yml \
      -i $ANSIBLE_INVENTORY \
      -l $ZOS_ENVIRONMENT \
      -e wd_deployment_plan_file=$outputDir/deploymentPlan.yaml \
      -e wd_package_file=$outputDir/applicationArchive.tar \
      -e hlq=$TARGET_HLQ \
      -e application=$APPLICATION \
      -e deploy_cfg_home=$deploy_config_home \
      -e planTags=db2"""

    ${CMD} | tee ${outputDir}/02-wazideploy-ansible-deploy-db2-steps.log
    rc=$?

    if [ $rc -eq 0 ]; then
        echo "[INFO] - ansible-playbook deploy completed."
    else
        echo "[WARNING] - ansible-playbook deploy failed."
    fi
fi

if [ $rc -eq 0 ]; then
    echo "[INFO] - ansible-playbook deploy skip db2 tag."

    CMD="""ansible-playbook deploy.yml \
      -i $ANSIBLE_INVENTORY \
      -l $ZOS_ENVIRONMENT \
      -e wd_deployment_plan_file=$outputDir/deploymentPlan.yaml \
      -e wd_package_file=$outputDir/applicationArchive.tar \
      -e hlq=$TARGET_HLQ \
      -e application=$APPLICATION \
      -e deploy_cfg_home=$deploy_config_home \
      -e planSkipTags=db2"""

    ${CMD} | tee ${outputDir}/03-wazideploy-ansible-deploy-remaining-steps.log
    rc=$?

    if [ $rc -eq 0 ]; then
        echo "[INFO] - ansible-playbook deploy completed."
    else
        echo "[WARNING] - ansible-playbook deploy failed."
    fi
fi
