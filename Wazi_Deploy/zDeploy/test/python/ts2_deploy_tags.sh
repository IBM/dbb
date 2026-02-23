#!/bin/env sh

#
# Inputs
# packageUrl
#

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

zos_environment=int_a_zos_host

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

    echo "[INFO] - wazideploy-deploy using db2 tag."

    CMD="""wazideploy-deploy \
      --workingFolder $outputDir \
      --deploymentPlan $outputDir/deploymentPlan.yaml \
      --envFile ../../environment-configuration/python/EOLEB7-Integration.yml \
      -e application=$APPLICATION \
      -e hlq=$TARGET_HLQ \
      -e deploy_cfg_home=../../ \
      --packageInputFile $outputDir/applicationArchive.tar \
      --evidencesFileName $evidenceDir/evidence.yaml \
      --planTags db2"""

    ${CMD} | tee ${outputDir}/02-wazideploy-deloy-db2-steps.log
    rc=$?

    if [ $rc -eq 0 ]; then
        echo "[INFO] - wazideploy-deploy completed."
    else
        echo "[WARNING] - wazideploy-deploy failed."
    fi
fi

if [ $rc -eq 0 ]; then
    echo "[INFO] - wazideploy-deploy skip db2 tag."

    CMD="""wazideploy-deploy \
      --workingFolder $outputDir \
      --deploymentPlan $outputDir/deploymentPlan.yaml \
      --envFile ../../environment-configuration/python/EOLEB7-Integration.yml \
      -e application=$APPLICATION \
      -e hlq=$TARGET_HLQ \
      -e deploy_cfg_home=../../ \
      --packageInputFile $outputDir/applicationArchive.tar \
      --evidencesFileName $evidenceDir/evidence.yaml \
      --planSkipTags db2"""

    ${CMD} | tee ${outputDir}/03-wazideploy-deloy-remaining-steps.log
    rc=$?

    if [ $rc -eq 0 ]; then
        echo "[INFO] - wazideploy-deploy completed."
    else
        echo "[WARNING] - wazideploy-deploy failed."
        exit 1
    fi
fi
