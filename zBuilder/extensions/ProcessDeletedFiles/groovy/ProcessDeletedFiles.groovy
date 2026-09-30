@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript
import com.ibm.dbb.build.CopyToPDS
import com.ibm.dbb.build.report.BuildReportFactory
import com.ibm.dbb.build.report.records.AnyTypeRecord
import com.ibm.dbb.task.TaskConstants
import com.ibm.jzos.ZFile

/*
* This custom task Groovy script provides compatibility with Wazi Deploy for deleted
* files. It processes deleted files from incremental builds by creating "DELETE_RECORD"
* AnyTypeRecords for each deleted file added to the context by the ImpactAnalysis task. 
* The output PDSs are determined from task configuration in the YAML file. In addition
* record generation, the script will also delete the corresponding members from the 
* build output datasets.
*
* See ProcessDeletedFiles.yaml for a sample of the required task configuration.
*/

// check there are deleted files to process
Set<String> deletedFiles = context.getSetStringVariable(TaskConstants.DELETED_FILES);
if (deletedFiles == null || deletedFiles.isEmpty()) {
   println ">> No deleted files to process."
   return 0
}

// process each deleted file
deletedFiles.each { String deletedFile ->
    // Create a new AnyTypeRecord for the deleted file
    AnyTypeRecord deleteRecord = new AnyTypeRecord("DELETE_RECORD")
    deleteRecord.setAttribute("file", deletedFile)

    List<String> deletedOutputsList = new ArrayList<String>() 

    String member = CopyToPDS.createMemberName(deletedFile)
    // get outputLibs for deleted file from config mappings
    List<String> outputLibs = config.getListStringFileVariable("outputLibs", deletedFile)
    if (outputLibs != null && !outputLibs.isEmpty()) {
        outputLibs.each { String outputLib ->
            // add fully qualified dsn to deleted outputs list
            String pdsMember = "${outputLib}(${member})"
            deletedOutputsList.add(pdsMember)
            println "> Document deletion ${pdsMember} for file ${deletedFile}"
            
            // delete member from build datasets
            if (ZFile.dsExists("//'$pdsMember'")) {
                println ">> Deleting ${pdsMember}"
                ZFile.remove("//'$pdsMember'")
            }
        }
    } else {
        println "> No outputLibs configured for file ${deletedFile}. No record created."
    }

    
    if (deletedOutputsList.size() > 0 ) { 
        deleteRecord.setAttribute("deletedBuildOutputs", deletedOutputsList)
        BuildReportFactory.getBuildReport().addRecord(deleteRecord)
    }
}

// orchestrator expects a return code of 0 for success
return 0