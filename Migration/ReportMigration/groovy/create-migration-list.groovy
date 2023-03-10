@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript

import groovy.transform.Field;
import groovy.cli.commons.CliBuilder;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Option;
import groovy.cli.commons.OptionAccessor;
import java.util.stream.Collectors;
import java.util.function.Function;

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

    // Process CLI arguments
    OptionAccessor options = getOptions(args);
    if (options.debug) {
        this.debug = true;
        connectionScript.setDebug(true);
    }

    File jsonFile = options.arguments()[0] as File;
    if (jsonFile.getParentFile() != null && jsonFile.getParentFile().exists() == false) {
        jsonFile.getParentFile().mkdirs();
    }

    // Instantiate RepositoryClient
    if (options.pw) {
        connectionScript.setClient(options.url, options.id, options.pw);
    } else {
        connectionScript.setClient(options.url, options.id, options.pwFile as File);
    }
    
    List<String> groups = collectGroups(options.grps ?: null, options.grpf ? options.grpf as File : null);
    List<String> resultGroups = connectionScript.getBuildResultGroups();
    // Match input groups to collection groups
    groups = matchGroups(resultGroups, groups);
    if (groups.size() == 0) {
        println("No groups matched.");
        System.exit(0);
    }

    // Filter by groups and script tag presence
    def results = connectionScript.getNonStaticBuildResults(groups);
    if (results.size() == 0) {
        println("No non-static build reports found.");
        System.exit(0);
    }

    // Create JSON Object
    connectionScript.createMigrationList(jsonFile, results);
    println("Migration list '${jsonFile.getAbsolutePath()}' created.");

} catch (Exception error) {
    println(error.getMessage());
    System.exit(1);
}

/****************************
**  Argument Parsing       **
*****************************/

/**
 * Parses arguments, printing help and exiting if required.
 * 
 * @param args  The input arguments to parse.
 * @return      An OptionAccessor at which to access the parsed options.
 */
private OptionAccessor getOptions(String[] args) {
    // Parse arguments and choose proper initialization method
    String usage = "create-migration-list.sh <json-list> [options] [--help]";
    String header = "Using DBB version ${versionUtils.getVersion()}";
    CliBuilder parser = new CliBuilder(usage:usage, header:header, stopAtNonOption:false);

    parser.url(type:String, longOpt:'url', args:1, required:true, 'Repository Client URL. Example: https:<Repository Client location>');
    parser.id(type:String, longOpt:'id', args:1, required:true, 'Repository Client user id.');

    // Mutually exclusive
    // Groups do not support the type argument, so they must be cast to the proper type
    OptionGroup passwordGroup = new OptionGroup();
    passwordGroup.setRequired(true);
    passwordGroup.addOption(parser.option("pw", [type:String, longOpt:'pw', args:1], 'Repository Client password.'));
    passwordGroup.addOption(parser.option("pwFile", [type:File, longOpt:'pwFile', args:1], 'Repository Client password file.'));
    parser.options.addOptionGroup(passwordGroup);

    OptionGroup groupGroup = new OptionGroup();
    groupGroup.setRequired(true);
    groupGroup.addOption(parser.option("grp", [longOpt:"grp", args:Option.UNLIMITED_VALUES, valueSeparator:','], "A comma seperated list of build groups with support for wildcard '*' matching."));
    groupGroup.addOption(parser.option("grpf", [type:File, longOpt:"grpf", args:1], "A file containing build groups seperated by new lines with support for wildcard '*' matching."));
    parser.options.addOptionGroup(groupGroup);
    
    parser.help(longOpt:'help', 'Prints this message.');
    parser.debug(longOpt:'debug', 'Prints groups and reports that are skipped.');
    
    OptionAccessor options = parser.parse(args);
    if (options == null) System.exit(1);
    if (options.arguments().size() == 0) {
        println("error: Positional argument, 'json-list', must be specified.");
        parser.usage();
        System.exit(1);
    }
    if (options.help) {
        parser.usage();
        System.exit(0);
    }
    return options;
}

/**
 * Collects groups from an input list and a file that seperates groups with newlines.
 * 
 * @param groupsArg     A list of groups passed in through the CLI, can be null.
 * @param groupsFileArg A file containing groups seperated by newlines.
 * @return              the list of groups gathered.
 */
private List<String> collectGroups(List<String> groupsArg, File groupsFileArg) {
    List<String> groups = new ArrayList<>();
    // Parses from both items
    if (groupsArg != null) {
        for (String group : groupsArg) {
            group = group.trim();
            if (group.isEmpty()) continue;
            groups.add(group);
        }
    }
    
    if (groupsFileArg != null) {
        groupsFileArg.eachLine { group ->
            group = group.trim();
            if (group.isEmpty()) return;
            if (groups.contains(group) == false) {
                groups.add(group);
            }
        }
    }

    return groups;
}

/**
 * Match wildcard and exact groups against a list of groups
 * 
 * @param resultGroups  The groups to match against.
 * @param groups        The groups to look for.
 * @return              A list of groups that are present in the resultGroups list.
 */
private List<String> matchGroups(List<String> resultGroups, List<String> groups) {
    // Sort group list from longest to shortest to match the most specific entries first
    groups.sort(Comparator.comparingInt(String::length).reversed());

    // Return the entire set if '*' is passed in as a group
    List<String> matchedGroups = new ArrayList<>();
    if (groups.contains("*")) {
        if (this.debug) {
            println("Wildcard group entered, all results selected.");
            if (groups.size() > 1) {
                groups.remove("*");
                println("Groups '${String.join(', ', groups)}' skipped.");
            }
        }
        matchedGroups.addAll(resultGroups);
        return matchedGroups;
    }

    for (String group : groups) {
        if (group.contains("*")) { // Match wildcard groups
            // Transform to ArrayList to add .remove() functionality
            List<String> matchItems = new ArrayList<>(Arrays.asList(group.split("\\*")));
            
            // Creates a map where the key is the group name, and the value is the text left to match
            Map<String, String> partialMatches = resultGroups.stream().filter(match -> {
                return group.startsWith("*") ? match.contains(matchItems.get(0)) : match.startsWith(matchItems.get(0));
            }).collect(Collectors.toMap(Function.identity(), match -> { // Remove matched string from value
                return match.substring(match.indexOf(matchItems.get(0)) + matchItems.get(0).length());
            }));
            matchItems.remove(0);

            // Iterate over the items left to match
            for (String matchText : matchItems) {
                // Same functions as the stream used to make the initial set, but different types and no handling for initial wildcard (*word)
                partialMatches = partialMatches.entrySet().stream().filter(entry -> {
                    return entry.getValue().contains(matchText);
                }).collect(Collectors.toMap(Map.Entry::getKey, entry -> { // Remove matched string from value
                    return entry.getValue().substring(entry.getValue().indexOf(matchText) + matchText.length());
                }));
            }

            // Remove items with remaining text to match if there is not a wildcard (*) at the end
            if (group.endsWith("*") == false) {
                partialMatches = partialMatches.entrySet().stream().filter(entry -> entry.getValue().isEmpty()).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            }
            // Add to the matched groups list
            partialMatches.keySet().forEach(key -> {
                resultGroups.remove(key);
                matchedGroups.add(key);
            });

            if (partialMatches.isEmpty() && this.debug) {
                println("Group '$group' did not match any stored result groups.");
            }

        } else { // Exact match groups
            if (resultGroups.contains(group)) {
                // Remove from result groups to avoid duplicates
                resultGroups.remove(group);
                matchedGroups.add(group);
            } else if (this.debug) {
                println("Group '$group' did not match any stored result groups.");
            }
        }
    }
    
    return matchedGroups
}