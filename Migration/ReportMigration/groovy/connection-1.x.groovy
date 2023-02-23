@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript

import com.ibm.dbb.repository.RepositoryClient;
import com.ibm.dbb.repository.BuildResult;
import com.ibm.dbb.repository.internal.PasswordUtil;
import com.ibm.dbb.build.internal.Utils;
import com.ibm.dbb.build.BuildProperties;
import com.ibm.dbb.build.report.BuildReport;

import groovy.transform.Field;
import java.nio.file.Path;
import java.nio.file.Files;
import groovy.cli.commons.CliBuilder;

import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;

@Field RepositoryClient client = null;
@Field List<String> groups = new ArrayList<>();

/****************************
**  Argument Parsing       **
*****************************/

boolean parseArgsInstantiate(String[] args, String version) {
    // Parse arguments and choose proper initialization method
    // Returns true if options are successfully parsed and false if not
    String usage = "static-report-migration.groovy [options]";
    String header = "Using DBB version ${version}";
    CliBuilder parser = new CliBuilder(usage:usage, header:header);

    parser.url(type:String, longOpt:'url', args:1, required:true, 'Repository Client URL. Example: https:<Repository Client location>');
    parser.id(type:String, longOpt:'id', args:1, required:true, 'Repository Client user id.');

    // Mutually exclusive
    OptionGroup passwordGroup = new OptionGroup();
    passwordGroup.setRequired(true);
    passwordGroup.addOption(parser.option("pw", [type:String, longOpt:'pw', args:1], 'Repository Client password.'));
    passwordGroup.addOption(parser.option("pwFile", [type:File, longOpt:'pwFile', args:1], 'Repository Client password file.'));

    OptionGroup groupGroup = new OptionGroup();
    groupGroup.setRequired(true);
    groupGroup.addOption(parser.option("grp", [longOpt:"grp", args:Option.UNLIMITED_VALUES, valueSeparator:','], "A comma seperated list of groups."));
    groupGroup.addOption(parser.option("grpf", [type:File, longOpt:"grpf", args:1], "A file containing groups seperated by new lines."));
    
    parser.help(longOpt:'help', 'Prints this message.');
    
    parser.options.addOptionGroup(passwordGroup);
    parser.options.addOptionGroup(groupGroup);
    def options = parser.parse(args);
    
    if (options == null) return false;
    
    if (options.help) {
        parser.usage();
        return false;
    }
    
    setGroups(options.grps ?: null, options.grpf ? options.grpf as File : null);
    if (options.pw) {
        setClient(options.url, options.id, options.pw);
    } else {
        setClient(options.url, options.id, options.pwFile as File);
    }

    return true;
}

void setGroups(List<String> groupsArg, File groupsFileArg) {
    // Parses from both items
    if (groupsArg != null) {
        for (String group : groupsArg) {
            groups.add(group.trim());
        }
    }
    
    if (groupsFileArg != null) {
        groupsFileArg.eachLine { group ->
            group = group.trim();
            if (group.isEmpty()) return;
            // Remove trailing comma in case a CSV is passed in.
            if (group.endsWith(",")) group = group.substring(0, group.length()-1);
            if (groups.contains(group) == false) {
                groups.add(group);
            }
        }
    }
}

/****************************
**  Client Instantiation   **
*****************************/

// Repository client instantiation
void setClient(String url, String id, String password) {
    client = new RepositoryClient();
    client.forceSSLTrusted(true);
    client.setUrl(url);
    client.setUserId(id);
    client.setPassword(password);
}

void setClient(String url, String id, File passwordFile) {
    client = new RepositoryClient();
    client.forceSSLTrusted(true);
    client.setUrl(url);
    client.setUserId(id);
    client.setPasswordFile(passwordFile);
}

/****************************
**  Command Execution      **
*****************************/

void enableFileTagging() {
    // Set tagging for proper encoding on generated html files
    BuildProperties.setProperty(Utils.FILE_TAGGING_OPTION_NAME, "true");
}

List<BuildResult> getBuildResults() {
    // Multiple requests to avoid excess memory usage by returning all and then filtering
    List<BuildResult> results = new ArrayList<>();
    for (String group : groups) {
        results.addAll(client.getAllBuildResults(Collections.singletonMap(RepositoryClient.GROUP, group)));
    }
    return results;
}

void filterBuildResults(List<BuildResult> results) {
    results.removeIf(result->!Utils.readFromStream(result.fetchBuildReport(), "UTF-8").contains("</script>"));
}

void convertBuildReports(List<BuildResult> results) {
    for (BuildResult result : results) {
        Path html = Files.createTempFile("dbb-report-mig", ".html");
        BuildReport report = BuildReport.parse(result.fetchBuildReportData());
        report.generateHTML(html.toFile());
        result.setBuildReport(new FileInputStream(html.toFile()));
        result.save();
        println("${result.getGroup()}:${result.getLabel()} converted.");
        Files.delete(html);
    }
}