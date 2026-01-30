@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript
import com.ibm.dbb.metadata.MetadataStoreFactory
import com.ibm.dbb.task.TaskConstants
import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.html.*
import com.ibm.dbb.build.report.records.*

/**
 * Document Reason to Build
 * 
 * Analyzes and reports on files that were changed vs files that were built due to dependencies.
 * Generates metadata records for each file with its build reason.
 */

// Retrieve changed files and the build list
Set changedFiles = context.getSetStringVariable(TaskConstants.CHANGED_FILES) ?: [] as Set
Set buildFiles = context.getSetStringVariable(TaskConstants.BUILD_LIST) ?: [] as Set
boolean verbose = context.getBooleanVariable(TaskConstants.IS_VERBOSE_MODE)

if (verbose) println "> Document reason for building"

// Compare the two lists and generate report
if (verbose) {
    println "=== Build Analysis Report ==="
    println "=" * 50
}

// Files that are on the CHANGED_FILES list
println "> Changed Files: ${changedFiles.size()}"
changedFiles.each { file ->
    generateMetadataRecord(file, "changed")
}
if (verbose) printFileList(changedFiles)

// b) Files that are on the BUILD_LIST but not on the CHANGED_FILES list (impacted files)
Set impactedFiles = buildFiles - changedFiles
println "> Impacted Files (built due to dependencies): ${impactedFiles.size()}"
impactedFiles.each { file ->
    generateMetadataRecord(file, "impacted")
}
if (verbose) printFileList(impactedFiles)

// Summary statistics
if (verbose) {
    println "=== Summary ==="
    println "-" * 50
    println "Total Changed Files:    ${changedFiles.size()}"
    println "Total Files Built:      ${buildFiles.size()}"
    println "Impacted Files:         ${impactedFiles.size()}"
    println "Changed & Built:        ${(changedFiles.intersect(buildFiles)).size()}"
    println "=" * 50
}

return 0

/**
 * Generate a metadata record for a file with its build reason
 * @param file The file path
 * @param reason The reason for building (changed or impacted)
 */
def generateMetadataRecord(String file, String reason) {
    PropertiesRecord filePropertiesMetadataRecord = new PropertiesRecord()
    filePropertiesMetadataRecord.addProperty("file", file)
    filePropertiesMetadataRecord.addProperty("reasonForBuilding", reason)
    BuildReportFactory.getBuildReport().addRecord(filePropertiesMetadataRecord)
}

/**
 * Print file list in verbose mode
 * @param files Set of files to print
 */
def printFileList(Set files) {
        println "-" * 50
        if (files.isEmpty()) {
            println "   No files in this category"
        } else {
            files.each { file ->
                println "   ${file}"
            }
        }
}
