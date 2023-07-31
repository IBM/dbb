import com.ibm.dbb.extensions.scanners.dtl.DTLDependencyScanner
import com.ibm.dbb.dependency.*

/*
 * This script  and the sample test program nede
 * to go on USS to invoke the new scanner
 *
 * Invoke this script via groovyz:
 *
 * groovyz  -cp /u/ibmzser/dbb-scanner/com.dat.dtl.dbb.scanner.jar dependencyScanner.groovy
 *
 */

String buildFile = "FLMB#P.dtlenu"
String workspace = "/u/ibmuser/userBuild"

// creating own scanner
def scanner = new DTLDependencyScanner()

// scanning file
LogicalFile logicalFile = scanner.scan(buildFile, workspace)

//listing file
println logicalFile