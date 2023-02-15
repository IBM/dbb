@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript

import com.ibm.dbb.build.VersionInfo;

String leastAcceptableVersion = "1.1.4";
String mostAcceptableVersion = "2.0.0";
String version = VersionInfo.getInstance().getVersion();
if (!isVersionUnder(version, leastAcceptableVersion)) {
    println(String.format("DBB Version %s is not compatable with this tool, please upgrade to version >= %s", version, leastAcceptableVersion));
    return 1;
}
if (!isVersionOver(version, mostAcceptableVersion)) {
    println(String.format("DBB Version %s is not compatable with this tool, please use the version of this tool compatable with DBB 2.x", version));
    return 1;
}


def connectionScript = loadScript(new File("connection-1.x.groovy"));


// Parse arguments and instantiate client
if (!connectionScript.parseArgsInstantiate(args, version)) {
    return 1;
}

// Ensure tagging on generated html files
connectionScript.enableFileTagging();

def results = connectionScript.getBuildResults();
connectionScript.filterBuildResults(results);

if (results.size() == 0) {
    println("No non-static build reports found.")
} else {
    print("You are about to convert ${results.size()} reports. Would you like to proceed ('y' or 'n'): ")
    // Works where there is no Console instance
    String response = System.in.newReader().readLine().trim().toLowerCase();
    if (response.equals("y") || response.equals("yes")) {
        connectionScript.convertBuildReports(results);
    }
}

println("Success");


/****************************
**  Utility                **
*****************************/

boolean isVersionUnder(String version, String leastVersion) {
    int[] versionArr = Arrays.stream(version.split("\\.")).mapToInt(Integer::parseInt).toArray();
    int[] leastAcceptableVersion = Arrays.stream(leastVersion.split("\\.")).mapToInt(Integer::parseInt).toArray(); // Inclusive
    

    // malformed version, expecting at most 3 version values
    if (versionArr.length > 3) return false;
    if (leastAcceptableVersion.length > 3) return false;

    // Ensure versions are of equal length
    while (versionArr.length < 3) {
        versionArr.append(0);
    }
    while (leastAcceptableVersion.length < 3) {
        leastAcceptableVersion.append(0)
    }
    
    // Reject for least acceptable version
    if (versionArr[0] < leastAcceptableVersion[0]) return false;
    if (versionArr[0] == leastAcceptableVersion[0]) {
        if (versionArr[1] < leastAcceptableVersion[1]) return false;
        if (versionArr[1] == leastAcceptableVersion[1] && versionArr[2] < leastAcceptableVersion[2]) return false;
    }

    return true;
}

boolean isVersionOver(String version, String mostVersion) {
    int[] versionArr = Arrays.stream(version.split("\\.")).mapToInt(Integer::parseInt).toArray();
    int[] mostAcceptableVersion = Arrays.stream(mostVersion.split("\\.")).mapToInt(Integer::parseInt).toArray(); // Non-inclusive

    // malformed version, expecting at most 3 version values
    if (versionArr.length > 3) return false;
    if (mostAcceptableVersion.length > 3) return false;

    // Ensure versions are of equal length
    while (versionArr.length < 3) {
        versionArr.append(0);
    }
    while (mostAcceptableVersion.length < 3) {
        mostAcceptableVersion.append(0)
    }

    // Reject for most acceptable version
    if (versionArr[0] > mostAcceptableVersion[0]) return false;
    if (versionArr[0] == mostAcceptableVersion[0]) {
        if (versionArr[1] > mostAcceptableVersion[1]) return false;
        if (versionArr[1] == mostAcceptableVersion[1] && versionArr[2] >= mostAcceptableVersion[2]) return false;
    }
    return true;
}