#!/bin/env sh

# Test script
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SCRIPT_NAME=$(basename "$0")_logs

# Load Wazi Deploy configuration
. ${SCRIPT_DIR}/env.sh

# Outputs dirs
timestamp=$(date +%F_%H-%M-%S)
outputDir=${SCRIPT_DIR}/$SCRIPT_NAME/${timestamp}
evidenceDir=${outputDir}/evidences

# Folder setup
rm -Rf ${SCRIPT_DIR}/${SCRIPT_NAME}

mkdir -p $outputDir
mkdir -p $evidenceDir

#
# Generate
#

echo "[INFO] - wazideploy-genereate."
CMD="""wazideploy-generate \
 --deploymentMethod $DEPLOYMENT_METHOD \
 --deploymentPlan $outputDir/deploymentPlan.yaml \
 --deploymentPlanReport $outputDir/deploymentPlanReport.html \
 --packageInputFile $PACKAGE_URL \
 --packageOutputFile $outputDir/applicationArchive.tar \
 --configFile $WAZI_DEPLOY_CONFIG_FILE_ANSIBLE """
echo "[INFO] Executing following command : $CMD"
${CMD} | tee ${outputDir}/01-wazideploy-generate.log
rc=$?
if [ $rc -eq 0 ]; then
    echo "[INFO] - wazideploy-generate completed."
else
    echo "[WARNING] - wazideploy-generate failed."
    exit 1
fi


echo "[INFO] - ansible-playbook deploy using db2 tag."
export EVIDENCES_FOLDER=$evidenceDir
cd ../../environment-configuration/ansible/
CMD="""ansible-playbook deploy.yml \
  -i $ANSIBLE_INVENTORY \
  -l $ZOS_ENVIRONMENT \
  -e wd_deployment_plan_file=$outputDir/deploymentPlan.yaml \
  -e wd_package_file=$outputDir/applicationArchive.tar \
  -e hlq=$TARGET_HLQ \
  -e application=$APPLICATION \
  -e deploy_cfg_home=$DEPLOYMENT_CONFIG_HOME \
  -e planTags=db2"""
${CMD} | tee ${outputDir}/02-wazideploy-ansible-deploy-db2-steps.log
rc=$?
if [ $rc -eq 0 ]; then
    echo "[INFO] - ansible-playbook deploy.yml completed."
else
    echo "[WARNING] - ansible-playbook deploy.yml failed."
    exit 1
fi

echo "[INFO] - ansible-playbook deploy skip db2 tag."
CMD="""ansible-playbook deploy.yml \
  -i $ANSIBLE_INVENTORY \
  -l $ZOS_ENVIRONMENT \
  -e wd_deployment_plan_file=$outputDir/deploymentPlan.yaml \
  -e wd_package_file=$outputDir/applicationArchive.tar \
  -e hlq=$TARGET_HLQ \
  -e application=$APPLICATION \
  -e deploy_cfg_home=$DEPLOYMENT_CONFIG_HOME \
  -e planSkipTags=db2"""
${CMD} | tee ${outputDir}/03-wazideploy-ansible-deploy-remaining-steps.log
rc=$?
if [ $rc -eq 0 ]; then
    echo "[INFO] - ansible-playbook deploy.yml completed."
else
    echo "[WARNING] - ansible-playbook deploy.yml failed."
    exit 1
fi
