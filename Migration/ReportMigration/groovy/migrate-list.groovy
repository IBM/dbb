@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript

import groovy.transform.Field;
import groovy.cli.commons.CliBuilder;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Option;
import groovy.cli.commons.OptionAccessor;

@Field def versionUtils = loadScript(new File("check-version.groovy"));
@Field boolean debug = false;

String leastAcceptableVersion = "1.1.4";
String mostAcceptableVersion = "2.0.0";
// Check DBB Version
String errorMessage;
if ((errorMessage = versionUtils.checkVersion(leastAcceptableVersion, mostAcceptableVersion)) != null) {
    println(errorMessage);
    System.exit(1);
}

// Main execution block
try {
    def connectionScript = loadScript(new File("dbb-1.x-api.groovy"));

    // Parse arguments and instantiate client
    OptionAccessor options = getOptions(args);

    // Options passed validation
    if (options.debug) {
        this.debug = true;
        connectionScript.setDebug(true);
    }

    File jsonFile = options.arguments()[0] as File;

    // Intantiate Metadata Store
    if (options.props) {
        Properties props = new Properties();
        props.load(options.props);
        // Override url if its passed in
        if (options.url) {
            props.setProperty("url", options.url);
        }
        if (options.pw) {
            connectionScript.setStore(options.id, options.pw, props);
        } else {
            connectionScript.setStore(options.id, options.pwFile as File, props);
        }
    } else {
        if (options.pw) {
            connectionScript.setStore(options.url, options.id, options.pw);
        } else {
            connectionScript.setStore(options.url, options.id, options.pwFile as File);
        }
    }

    Map<String, List<String>> migrationList = connectionScript.readMigrationList(jsonFile);
    if (migrationList.size() == 0) {
        println("No build results found in '$jsonFile'.");
        System.exit(0);
    }

    // Ensure tagging on generated html files
    connectionScript.enableFileTagging();

    for (Map.Entry<String, List<String>> entry : migrationList) {
        def results = connectionScript.getBuildResultsFromGroup(entry.getKey(), entry.getValue());
        if (results.size() == 0) {
            if (this.debug) {
                println("No results matched for group '${entry.getKey()}'.");
            }
            continue;
        }
        connectionScript.convertBuildReports(results);
    }
} catch (Exception error) {
    println(error.getMessage());
    System.exit(1);
}

private OptionAccessor getOptions(String[] args) {
    String usage = "migrate-list.sh <json-file> [options] [--help]";
    String header = "Using DBB version ${versionUtils.getVersion()}";
    CliBuilder parser = new CliBuilder(usage:usage, header:header, stopAtNonOption:false);

    parser.id(type:String, longOpt:'id', args:1, required:true, 'Repository Client user id.');
    
    // One required but not mutually exclusive. URL can overwrite props "url" property.
    parser.url(type:String, longOpt:'url', args:1, required:true, 'Repository Client URL. Example: https:<Repository Client location>');

    // Mutually exclusive groups
    // Groups do not support the type argument, so they must be cast to the proper type.
    OptionGroup passwordGroup = new OptionGroup();
    passwordGroup.setRequired(true);
    passwordGroup.addOption(parser.option("pw", [type:String, longOpt:"pw", args:1], 'Repository Client password.'));
    passwordGroup.addOption(parser.option("pwFile", [type:File, longOpt:'pwFile', args:1], 'Repository Client password file.'));
    parser.options.addOptionGroup(passwordGroup);

    parser.help(longOpt:"help", 'Prints this message.');
    parser.debug(longOpt:"debug", 'Prints entries that are skipped.');
    
    // Should not display any output, just used to validate positional arguments
    
    def options = parser.parse(args);
    if (options == null) System.exit(1);
    if (options.arguments().size() == 0) {
        println("error: Positional argument, 'json-file', must be specified.");
        parser.usage();
        System.exit(1);
    }
    if ((options.arguments()[0] as File).isFile() == false) {
        println("error: Positional argument, 'json-file', must exist.");
        parser.usage();
        System.exit(1);
    }
    if (options.help) {
        parser.usage();
        System.exit(0);
    }
    return options;
}
