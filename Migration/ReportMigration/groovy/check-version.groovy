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