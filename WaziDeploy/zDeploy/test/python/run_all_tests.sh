#!/bin/env s

#
# Wazi Deploy Test Driver
# Runs all test scenarios and reports results
#

# Test script
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
timestamp=$(date +%F_%H-%M-%S)
RESULTS_DIR=${SCRIPT_DIR}/test_results/${timestamp}

# Create results directory
mkdir -p ${RESULTS_DIR}

# Initialize counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Test result log
RESULTS_LOG=${RESULTS_DIR}/test_results.log

# Print header
echo "========================================" | tee ${RESULTS_LOG}
echo "Wazi Deploy Test Suite" | tee -a ${RESULTS_LOG}
echo "Started: $(date)" | tee -a ${RESULTS_LOG}
echo "========================================" | tee -a ${RESULTS_LOG}
echo "" | tee -a ${RESULTS_LOG}

# Define test scripts
TEST_SCRIPTS="
ts1_full_deploy.sh
ts2_deploy_tags.sh
ts3_full_rollback.sh
ts4_partial_rollback.sh
"

# Run each test
for test_script in ${TEST_SCRIPTS}; do
    if [ -f "${SCRIPT_DIR}/${test_script}" ]; then
        TOTAL_TESTS=$((TOTAL_TESTS + 1))
        
        echo "----------------------------------------" | tee -a ${RESULTS_LOG}
        echo "Running: ${test_script}" | tee -a ${RESULTS_LOG}
        echo "Started: $(date +%H:%M:%S)" | tee -a ${RESULTS_LOG}
        
        # Run the test and capture output
        TEST_LOG=${RESULTS_DIR}/${test_script}.log
        
        if sh "${SCRIPT_DIR}/${test_script}" > ${TEST_LOG} 2>&1; then
            echo "Status: PASSED" | tee -a ${RESULTS_LOG}
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo "Status: FAILED" | tee -a ${RESULTS_LOG}
            FAILED_TESTS=$((FAILED_TESTS + 1))
            echo "See log: ${TEST_LOG}" | tee -a ${RESULTS_LOG}
        fi
        
        echo "Completed: $(date +%H:%M:%S)" | tee -a ${RESULTS_LOG}
        echo "" | tee -a ${RESULTS_LOG}
    else
        echo "WARNING: Test script not found: ${test_script}" | tee -a ${RESULTS_LOG}
    fi
done

# Print summary
echo "========================================" | tee -a ${RESULTS_LOG}
echo "Test Suite Summary" | tee -a ${RESULTS_LOG}
echo "========================================" | tee -a ${RESULTS_LOG}
echo "Total Tests:  ${TOTAL_TESTS}" | tee -a ${RESULTS_LOG}
echo "Passed:       ${PASSED_TESTS}" | tee -a ${RESULTS_LOG}
echo "Failed:       ${FAILED_TESTS}" | tee -a ${RESULTS_LOG}
echo "" | tee -a ${RESULTS_LOG}

if [ ${FAILED_TESTS} -eq 0 ]; then
    echo "Result: ALL TESTS PASSED" | tee -a ${RESULTS_LOG}
    exit_code=0
else
    echo "Result: SOME TESTS FAILED" | tee -a ${RESULTS_LOG}
    exit_code=1
fi

echo "" | tee -a ${RESULTS_LOG}
echo "Completed: $(date)" | tee -a ${RESULTS_LOG}
echo "Results saved to: ${RESULTS_DIR}" | tee -a ${RESULTS_LOG}
echo "========================================" | tee -a ${RESULTS_LOG}

exit ${exit_code}