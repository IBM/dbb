@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript

/*
* This step task Groovy script provides compatibility with Wazi Deploy for deleted
* files. It processes deleted files from incremental builds by creating "DELETE_RECORD"
* AnyTypeRecords for each deleted file added to the context by the ImpactAnalysis task. 
* The output PDSs are determined from task configuration in the YAML file. In addition
* record generation, the script will also delete the corresponding members from the 
* build output datasets.
*/

Set<String> deletedFiles = context.getSetStringVariable(TaskConstants.DELETED_FILES);
if (deletedFiles == null || deletedFiles.isEmpty()) {
   println "> No deleted files to process."
   return 0
}

deletedFiles.each { String deletedFile ->
    //println "Creating deletion record for file: ${deletedFile}"

    // Create a new AnyTypeRecord for the deleted file
    AnyTypeRecord deleteRecord = new AnyTypeRecord("DELETE_RECORD")
    deleteRecord.setAttribute("file", deletedFile)

    List<String> deletedOutputsList = new ArrayList<String>() 

    String member = CopyToPDS.createMemberName(deletedFile)
    List<String> outputLibs = config.getListStringFileVariable("outputLibs", deletedFile)
    if (outputLibs != null && !outputLibs.isEmpty()) {
        outputLibs.each { String outputLib ->
            // add fully qualified dsn to deleted outputs list
            String pdsMember = "${outputLib}(${member})"
            deletedOutputsList.add(pdsMember)
            println "> Document deletion ${pdsMember} for file ${deletedFile}"

            // delete member from build datasets
            if (ZFile.dsExists("//'$pdsMember'")) {
                if (props.verbose) println ">> Deleting ${pdsMember}"
                ZFile.remove("//'$pdsMember'")
            }
        }
    } else {
        println "> No outputLibs configured for file ${deletedFile}. No deletion records created."
    }

    
    if (deletedOutputsList.size() > 0 ) { 
        deleteRecord.setAttribute("deletedBuildOutputs", deletedOutputsList)
        BuildReportFactory.getBuildReport().addRecord(deleteRecord)
    }
}

