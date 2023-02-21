@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript

import com.ibm.dbb.build.VersionInfo;
import groovy.transform.Field;

@Field def versionUtils = loadScript(new File("check-version.groovy"));

String leastAcceptableVersion = "1.1.4";
String mostAcceptableVersion = "2.0.0";
String version = VersionInfo.getInstance().getVersion();
if (!versionUtils.isVersionUnder(version, leastAcceptableVersion)) {
    println(String.format("DBB Version %s is not compatable with this tool, please upgrade to version >= %s", version, leastAcceptableVersion));
    System.exit(1);
}
if (!versionUtils.isVersionOver(version, mostAcceptableVersion)) {
    println(String.format("DBB Version %s is not compatable with this tool, please use the version of this tool compatable with DBB 2.x", version));
    System.exit(1);
}


def connectionScript = loadScript(new File("connection-1.x.groovy"));

// Parse arguments and instantiate client
if (!connectionScript.parseArgsInstantiate(args, version)) {
    System.exit(2);
}

// Ensure tagging on generated html files
connectionScript.enableFileTagging();

def results = connectionScript.getBuildResults();
connectionScript.filterBuildResults(results);

if (results.size() == 0) {
    println("No non-static build reports found.")
} else {
    println("You are about to convert ${results.size()} reports. Would you like to proceed ('y' or 'n'): ")
    // Works where there is no Console instance
    String response = System.in.newReader().readLine().trim().toLowerCase();
    if (response.equals("y") || response.equals("yes")) {
        connectionScript.convertBuildReports(results);
        println("Finished conversion.");
    } else {
        println("Conversion skipped.");
    }
}