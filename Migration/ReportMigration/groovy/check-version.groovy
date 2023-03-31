import com.ibm.dbb.build.VersionInfo;

/**
 * Checks that the input version is equal to or greater than the input leastVersion.
 * 
 * @param version       The version number to check.
 * @param leastVersion  The inclusive version number to check against.
 * @return              True if the version is acceptable, false otherwise.
 */
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

/**
 * Checks that the input version is less than the input mostVersion.
 * 
 * @param version       The version number to check.
 * @param mostVersion  The exclusive version number to check against.
 * @return             True if the version is acceptable, false otherwise.
 */
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

/**
 * Retrieves the version from the DBB API and checks it against the input version strings.
 * 
 * @param leastAcceptableVersion    The least acceptable version (inclusive).
 * @param mostAcceptableVersion     The most acceptable version (exclusive).
 * @return                          A filled error message if a check failed, an empty string otherwise.
 */
String checkVersion(String leastAcceptableVersion, String mostAcceptableVersion) {
    String version = getVersion();
    String errorMessage;
    if (!isVersionUnder(version, leastAcceptableVersion)) {
        errorMessage = String.format("DBB Version %s is not compatable with this tool, please upgrade to version >= %s", version, leastAcceptableVersion);
    } else if (!isVersionOver(version, mostAcceptableVersion)) {
        errorMessage = String.format("DBB Version %s is not compatable with this tool, please use the version of this tool compatable with DBB %s", version, leastAcceptableVersion);
    }
    return errorMessage;
}

/**
 * Returns the DBB version from its API.
 * 
 * @return  the current version of DBB.
 */
String getVersion() {
    return VersionInfo.getInstance().getVersion();
}