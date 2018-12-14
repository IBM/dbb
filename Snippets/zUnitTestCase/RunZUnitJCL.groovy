import com.ibm.dbb.build.CopyToHFS
import com.ibm.dbb.build.DBBConstants
import com.ibm.dbb.build.JCLExec


/*********************************************************************************
 * The following sample shows how to use JCLExec API to execute a ZUnit and
 * display the results in the console.
 * 
 * This sample assumes that user has setup the ZUnit and a JCL to execute the 
 * ZUnit.
 * 
 * This sample requires:
 *    1.  The data set contains the JCL.
 *    2.  The data set contains the output of the ZUnit result.
 *    
 * Sample output:
 * 
 *    Running ZUnit in JCL 'HUNGLAM.ZUNIT.JCL(ZUNIT001)'
 *    The JCL Job HUNGLAM(JOB07890) completed with Max-RC CC 0004
 *    ****************** Module [AZUTC001] ******************
 *    Name:       AZUTC001 Test Case Written In COBOL
 *    Status:     FAIL
 *    Test cases: 4 (3 passed, 1 failed, 0 errors)
 *    Details: 
 *          AZUTC001.test001
 *          AZUTC001.test002
 *       x  AZUTC001.test003
 *          AZUTC001.test004
 * 
 *********************************************************************************/

/* DBB_CONF must be set for running JCLExec */
def confDir = System.getenv("DBB_CONF")

/* The data set contains the ZUnit JCL */
/* For example:  &HLQ.ZUNIT.JCL        */
def jclDataset = 

/* The ZUnit JCL */
/* For example:  ZUNIT001   */
def jclMember = 

/* The data set contains the output member */
/* For example:  &HLQ.ZUNIT.AZURES         */
def resultDataset = 

/* The output member */
/* For example:   AZURS001     */
def resultMember = 

println "Running ZUnit in JCL '$jclDataset($jclMember)'"

/* Execute the JCL */
def exec = new JCLExec()
int rc = exec.dataset(jclDataset).member(jclMember).confDir(confDir).execute()

/* Check the return code from the submitted job */
def maxRC = exec.maxRC
if (maxRC == 'CC 0000')
    println "The JCL Job ${exec.submittedJobId} completed successfully"
else
    println "The JCL Job ${exec.submittedJobId} completed with Max-RC: $maxRC"

/* Copy the result from the data set member to a temporary file on zFS to parse the XML */    
def resultFile = File.createTempFile(resultMember, 'xml')    
def copyRc = new CopyToHFS().dataset(resultDataset).member(resultMember).file(resultFile).copyMode(DBBConstants.CopyMode.BINARY).execute()
    
if (copyRc != 0)
{
    println "Error copying result file from ${resultDataset($resultMember)} to $resultFile"
    System.exit(1)
}

/* Parsing the result file and display summary of the result */
def runnerResult = new XmlParser().parse(resultFile)
def testCase = runnerResult.testCase
println "****************** Module ${testCase.@moduleName} ******************"
println "Name:       ${testCase.@name[0]}"
println "Status:     ${statusToString(testCase.@result[0])}"
println "Test cases: ${testCase.@tests[0]} (${testCase.@passed[0]} passed, ${testCase.@failures[0]} failed, ${testCase.@errors[0]} errors)"
println "Details: "

testCase.test.each { test ->
    if (test.@result == 'pass')
        print '    '
    else
        print ' x  '    
    println "${test.@name}"
}


/**
 * A helper to display the status as either PASS or FAIL
 * @param status the status from the zunit runner
 * @return a string representing the status
 */
static def statusToString(def status)
{
    return status.equalsIgnoreCase('pass') ? 'PASS' : 'FAIL'
}