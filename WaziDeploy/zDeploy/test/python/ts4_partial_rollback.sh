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
 --configFile $WAZI_DEPLOY_CONFIG_FILE_PYTHON """
echo "[INFO] Executing following command : $CMD"
${CMD} | tee ${outputDir}/01-wazideploy-generate.log
rc=$?

if [ $rc -eq 0 ]; then
    echo "[INFO] - wazideploy-generate completed."
else
    echo "[WARNING] - wazideploy-generate failed."
    exit 1
fi

#
# Deploy
#
echo "[INFO] - wazideploy-deploy."
CMD="""wazideploy-deploy \
 --workingFolder $outputDir \
 --deploymentPlan $outputDir/deploymentPlan.yaml \
 --envFile ../../environment-configuration/python/EOLEB7-Integration.yml \
 -e application=$APPLICATION \
 -e hlq=$TARGET_HLQ \
 -e deploy_cfg_home=../../ \
 --packageInputFile $outputDir/applicationArchive.tar \
 --evidencesFileName $evidenceDir/evidence.yaml"""
${CMD} | tee ${outputDir}/02-wazideploy-deloy.log
rc=$?
if [ $rc -eq 0 ]; then
    echo "[INFO] - wazideploy-deploy completed."
else
    echo "[WARNING] - wazideploy-deploy failed."
    exit 1
fi

echo "[INFO] - wazideploy-deploy partial rollback."
CMD="""wazideploy-deploy \
 --workingFolder $outputDir \
 --deploymentPlan $outputDir/deploymentPlan.yaml \
 --envFile ../../environment-configuration/python/EOLEB7-Integration.yml \
 --planTags restore \
 -e application=$APPLICATION \
 -e hlq=$TARGET_HLQ \
 -e {\"includes_artifacts\":[\".*BAD1.JCL\"]} \
 -e deploy_cfg_home=../../ \
 --packageInputFile $outputDir/applicationArchive.tar \
 --evidencesFileName $evidenceDir/evidence.yaml"""
${CMD} | tee ${outputDir}/03-wazideploy-deloy-partial-rollback.log
rc=$?
if [ $rc -eq 0 ]; then
    echo "[INFO] - wazideploy-deploy completed."
else
    echo "[WARNING] - wazideploy-deploy failed."
    exit 1
fi
