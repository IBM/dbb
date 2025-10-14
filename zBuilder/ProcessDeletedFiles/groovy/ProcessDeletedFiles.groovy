@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript

/*
* This step task Groovy script provides compatibility with Wazi Deploy for deleted
* files. It processes deleted files from incremental builds by creating "DELETE_RECORD"
* AnyTypeRecords for each deleted file added to the context by the ImpactAnalysis task. 
* The output PDSs are determined from task configuration in the YAML file. In addition
* record generation, the script will also delete the corresponding members from the 
* build output datasets.
*/

// check there are deleted files to process
Set<String> deletedFiles = context.getSetStringVariable(TaskConstants.DELETED_FILES);
if (deletedFiles == null || deletedFiles.isEmpty()) {
   println ">> No deleted files to process."
   return 0
}

// parse output library mappings from config
Map<PathMatcher, List<String>> libMappings = parseOutputLibraryMappings(config.getListVariable("outputLibMappings"))
if (outputLibMappings == null || outputLibMappings.isEmpty()) {
        println ">> ERROR: 'outputLibMappings' not defined in task configuration."
        return -1
}

// process each deleted file
deletedFiles.each { String deletedFile ->
    // create a new AnyTypeRecord for the deleted file
    AnyTypeRecord deleteRecord = new AnyTypeRecord("DELETE_RECORD")
    deleteRecord.setAttribute("file", deletedFile)

    // gather deleted outputs
    List<String> deletedOutputsList = new ArrayList<String>() 
    String member = CopyToPDS.createMemberName(deletedFile)
    
    libMappings.keySet().each { PathMatcher matcher ->
        // find source match for deleted file 
        if (matcher.matches(Paths.get(deletedFile))) {
            List<String> outputLibs = libMappings.get(matcher)
            // document applicable output libraries for deleted file
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
                println "> No output library configured for file ${deletedFile}. No record created."
            }
        }
    }

    
    if (deletedOutputsList.size() > 0 ) { 
        deleteRecord.setAttribute("deletedBuildOutputs", deletedOutputsList)
        BuildReportFactory.getBuildReport().addRecord(deleteRecord)
    }
}



// helper function to parse output library mappings from config
Map<PathMatcher, List<String>> parseOutputLibraryMappings(List<Object> rawMaps) {
    // get outputLibMappings from config
    if (rawMaps == null || rawMaps.isEmpty()) {
        return null
    }
    // parse into map of source to list of outputLibs
    Map<PathMatcher, List<String>> libMappings = new LinkedHashMap<>()
    try {
        rawMaps.each { Object rawMap ->
            Map map = (Map) rawMap
            String source = map.get("source")
            List<String> outputLibs = map.get("outputLibs")
            if (source != null && outputLibs != null && !outputLibs.isEmpty()) {
                PathMatcher pathMatcher = getPathMatcher(source)
                libMappings.put(pathMatcher, outputLibs)
            } else {
                println ">> WARNING: Invalid library mapping entry. 'source' or 'outputLibs' missing or empty."
            }
        }
    } catch (Exception e) {
        println ">> ERROR: Library mappings format invalid. Parsing error: ${e.getMessage()}"
        return null
    }
    return libMappings
}

// helper function to create PathMatcher from glob or regex pattern
PathMatcher getPathMatcher(String pattern) {
    if (!pattern.startsWith("glob:") && !pattern.startsWith("regex:"))
        pattern = "glob:" + pattern;
    return FileSystems.getDefault().getPathMatcher(pattern);
}