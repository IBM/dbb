@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript

import com.ibm.dbb.repository.RepositoryClient;
import com.ibm.dbb.repository.BuildResult;
import com.ibm.dbb.build.internal.Utils;
import com.ibm.dbb.build.BuildProperties;
import com.ibm.dbb.build.report.BuildReport;
import com.ibm.json.java.JSONArray;
import com.ibm.json.java.JSONObject;

import groovy.transform.Field;
import java.nio.file.Path;
import java.nio.file.Files;
import java.util.stream.Collectors;

@Field RepositoryClient client = null;
@Field boolean debug = false;

/****************************
**  Client Instantiation   **
*****************************/

/**
 * Instantiates a Repository Client.
 * 
 * @param url   The url of the repository client instance.
 * @param id    The id of the repository client instance.
 * @param password  The password for the repository client instance user.
 */
public void setClient(String url, String id, String password) {
    client = new RepositoryClient();
    client.forceSSLTrusted(true);
    client.setUrl(url);
    client.setUserId(id);
    client.setPassword(password);
    client.setErrorStatusCode(201);
}

/**
 * Instantiates a Repository Client.
 * {@link #setClient(String, String, String)}
 * 
 * @param url   The url of the repository client instance.
 * @param id    The id of the repository client instance.
 * @param password  The password for the repository client instance user.
 */
public void setClient(String url, String id, File passwordFile) {
    client = new RepositoryClient();
    client.forceSSLTrusted(true);
    client.setUrl(url);
    client.setUserId(id);
    client.setPasswordFile(passwordFile);
    client.setErrorStatusCode(201);
}

/****************************
**  Command Execution      **
*****************************/

/**
 * Enables file tagging to ensure proper saving and parsing of the generated temporary report files.
 */
public void enableFileTagging() {
    // Set tagging for proper encoding on generated html files
    BuildProperties.setProperty(Utils.FILE_TAGGING_OPTION_NAME, "true");
}

/**
 * Retrieves build results for a list of groups and removes items that are missing a build report or a '</script>' tag in their html content.
 * 
 * @param groups    The groups to retrieve build results for.
 * @return          A list of non-static build results.
 */
public List<BuildResult> getNonStaticBuildResults(List<String> groups) {
    List<BuildResult> results = retrieveBuildResults(groups);
    filterBuildResults(results);
    return results;
}

/**
 * Retrieves build results from a group that match the input labels.
 * 
 * @param group     The group to retrieve build results for.
 * @param labels    The build results to retrieve.
 * @return          A list of build results.
 */
public List<BuildResult> getBuildResultsFromGroup(String group, List<String> labels) {
    List<BuildResult> results = retrieveBuildResults(group);
    results.removeIf(result -> {
        return !labels.contains(result.getLabel());
    });
    return results;
}

/**
 * Regenerates the HTML for the input build results.
 * 
 * @param results   A list of build results to regenerate HTML for.
 */
public void convertBuildReports(List<BuildResult> results) {
    for (BuildResult result : results) {
        Path html = Files.createTempFile("dbb-report-mig", ".html");
        
        BuildReport report = BuildReport.parse(result.fetchBuildReportData());
        report.generateHTML(html.toFile());
        result.setBuildReport(new FileInputStream(html.toFile()));
        result.save();
        
        System.out.println(String.format("Result '%s:%s' converted.", result.getGroup(), result.getLabel()));
        Files.delete(html);
    }
}

/**
 * Returns all of the build result groups from the repository client instance.
 */
public List<String> getBuildResultGroups() {
    return client.listBuildResultGroups();
}

/**
 * Retrieves and collects a list of build results from multiple groups.
 * {@link #retrieveBuildResults(String)}
 * @param groups    The list of groups to retrieve results for.
 * @return          The collected list of build results.
 */
private List<BuildResult> retrieveBuildResults(List<String> groups) {
    // Multiple requests to avoid excess memory usage by returning all and then filtering
    List<BuildResult> results = new ArrayList<>();
    for (String group : groups) {
        results.addAll(retrieveBuildResults(group));
    }
    return results;
}

/**
 * Retrieves build results from a single group.
 * 
 * @param group     The group to retrieve results for.
 * @return          The collected list of build results.
 */
private List<BuildResult> retrieveBuildResults(String group) {
    return client.getAllBuildResults(Collections.singletonMap(RepositoryClient.GROUP, group));
}

/**
 * Filters out build results inplace that are missing report content, or a '</script>' tag from their HTML.
 * This is to create a build result list only including non-static Build Reports.
 * 
 * @param results   The results to filter.
 */
private void filterBuildResults(List<BuildResult> results) {
    results.removeIf(result-> {
        InputStream buildReport = result.fetchBuildReport();
        if (buildReport == null) {
            if (debug) {
                System.out.println(String.format("Result '%s:%s' has no report... Skipping.", result.getGroup(), result.getLabel()));
            }
            return true;
        }

        String content = Utils.readFromStream(buildReport, "UTF-8");
        if (content == null) {
            if (debug) {
                System.out.println(String.format("Result '%s:%s' has no content... Skipping.", result.getGroup(), result.getLabel()));
            }
            return true;
        } else if (content.contains("</script>") == false) {
            if (debug) {
                System.out.println(String.format("Result '%s:%s' has no script tag... Skipping.", result.getGroup(), result.getLabel()));
            }
            return true;
        }
        return false;
    });
}

/****************************
**  Utilities              **
*****************************/

/**
 * Sets the debug state of this script.
 * 
 * @param on    The desired debug state.
 */
public void setDebug(boolean on) {
    this.debug = on;
}

/**
 * Creates a json formatted migration list with the input arguments.
 * 
 * @param jsonFile  The location to create the migration list at.
 * @param results   The results to include in the list.
 */
public void createMigrationList(File jsonFile, List<BuildResult> results) {
    // Create JSON Object
    JSONObject json = new JSONObject();
    for (BuildResult result : results) {
        if (json.containsKey(result.getGroup())) {
            JSONArray list = json.get(result.getGroup());
            list.add(result.getLabel());
        } else {
            JSONArray list = new JSONArray();
            list.add(result.getLabel());
            json.put(result.getGroup(), list);
        }
    }
    // Write JSON to file
    try (BufferedWriter writer = new BufferedWriter(new FileWriter(jsonFile))) {
        writer.write(json.serialize(true));
    }
}

/**
 * Returns a Map reflecting the state of the input migration list.
 * 
 * @param jsonFile  The migration list to read.
 * @return          A Map containing the info from the input json file.
 */
public Map<String, List<String>> readMigrationList(File jsonFile) {
    JSONObject json;
    try (BufferedReader reader = new BufferedReader(new FileReader(jsonFile))) {
        json = JSONObject.parse(reader);
    }

    Map<String, List<String>> list = json.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    return list;
}