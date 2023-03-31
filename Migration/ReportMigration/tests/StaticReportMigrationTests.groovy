import com.ibm.dbb.repository.RepositoryClient;
import com.ibm.dbb.repository.BuildResult;
import com.ibm.dbb.EnvVars;
import com.ibm.dbb.build.internal.Utils;
import com.ibm.json.java.JSONObject;
import com.ibm.dbb.build.VersionInfo;

import java.util.concurrent.TimeUnit;
import java.lang.Thread;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.ClassOrderer;
import org.junit.jupiter.api.TestClassOrder;

import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertIterableEquals;

@TestClassOrder(ClassOrderer.OrderAnnotation.class)
class StaticReportMigrationTests {
    private static final String GROUP = "Static-Report-Migration-Test";
    private static final String GROUP2 = "Static-Report-Migration-Test-2";
    private static final String LABEL = "buildresult";
    private static final String URL_KEY = "test-url";
    private static final String ID_KEY = "test-id";
    private static final String PW_FILE_KEY = "test-pwFile";

    private static File testDir = new File(StaticReportMigrationTests.getProtectionDomain().getCodeSource().getLocation().getPath()).getParentFile();
    private static String listScript = new File(testDir, "../bin/create-migration-list.sh").getPath();
    private static String migrateScript = new File(testDir, "../bin/migrate-list.sh").getPath();
    private static File jsonFile = new File("list.json");

    private static String url;
    private static String id;
    private static File passwordFile;
    private static RepositoryClient client;

    private static final int maxNormError = 202;
    private static final int maxDeleteError = 405;

    @BeforeAll
    static void setupStore() throws IOException {
        System.out.println("Setting up store.");
        if (System.getProperties().containsKey(URL_KEY) == false) {
            fail(String.format("Missing URL system property '%s'.", URL_KEY));
        }
        if (System.getProperties().containsKey(ID_KEY) == false) {
            fail(String.format("Missing ID system property '%s'.", ID_KEY));
        }
        if (System.getProperties().containsKey(PW_FILE_KEY) == false) {
            fail(String.format("Missing Password File system property '%s'.", PW_FILE_KEY));
        }

        url = System.getProperty(URL_KEY);
        id = System.getProperty(ID_KEY);
        passwordFile = new File(System.getProperty(PW_FILE_KEY));

        client = new RepositoryClient();
        client.forceSSLTrusted(true);
        client.setUrl(url);
        client.setUserId(id);
        client.setPasswordFile(passwordFile);
        client.setErrorStatusCode(maxNormError);

        setupCollection();
    }

    @AfterAll
    static void cleanupStore() {
        System.out.println("Cleaning up store.");
        client.setErrorStatusCode(maxDeleteError);
        client.deleteBuildResults(GROUP);
        client.deleteCollection(GROUP);
        client.deleteBuildResults(GROUP2);
        client.deleteCollection(GROUP2);
        jsonFile.delete();
    }

    @Test
    void testVersion() {
        System.out.println("Running version test.");
        File testVersion = new File(testDir, "samples/version.properties");

        File versionPackage = new File("com/ibm/dbb/build/internal/version.properties");
        versionPackage.getParentFile().mkdirs();
        Files.copy(testVersion.toPath(), versionPackage.toPath());
        String version = VersionInfo.getInstance().getVersion();
        VersionInfo.staticReset();
        try {
            // Update version file within jar
            List<String> command = new ArrayList<>();
            command.add("jar");
            command.add("-uf");
            command.add(EnvVars.getHome() + "/lib/dbb.core_" + version + ".jar");
            command.add(versionPackage.getPath());
            runProcess(command, 0);

            // Run tests on the create-list script
            String errorMessage = "DBB Version 1.1.3 is not compatable with this tool";
            command = new ArrayList<>();
            command.add(listScript);
            Map<String, String> output = runProcess(command, 1);
            assertTrue(output.get("out").contains(errorMessage));

            // Run test on the migration script
            command = new ArrayList<>();
            command.add(migrateScript);
            output = runProcess(command, 1);
            assertTrue(output.get("out").contains(errorMessage));
        } finally {
            // Replace the version file with the original
            versionPackage.delete();
            Files.copy(Paths.get(EnvVars.getHome() + "/bin/version.properties"), versionPackage.toPath());
            List<String> command = new ArrayList<>();
            command.add("jar");
            command.add("-uf");
            command.add(EnvVars.getHome() + "/lib/dbb.core_" + version + ".jar");
            command.add(versionPackage.getPath());
            try {
                runProcess(command, 0);
            } catch (Exception error) {
                throw new Exception("Failed to reset DBB version info back to original state.", error);
            } finally {
                // Clear temp folder structure
                while (versionPackage != null) {
                    versionPackage.delete();
                    versionPackage = versionPackage.getParentFile();
                }
            }
        }
        // Check version was reset properly
        assertEquals(VersionInfo.getInstance().getVersion(), version);
    }

    @Nested
    @TestMethodOrder(MethodOrderer.OrderAnnotation.class)
    @Order(1)
    class ListCreationTests {
        @Test
        @Order(1)
        void testWildcard() {
            System.out.println("Running group wildcard tests.");
            List<String> command = new ArrayList<>();
            command.add(listScript);
            command.add(jsonFile.getPath());
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pwFile");
            command.add(passwordFile.getPath());
            command.add("--grp");
            command.add("*");

            Map<String, String> output = runProcess(command, 0);
            Map<String, List<String>> expected = new HashMap<>();
            expected.put(GROUP, Arrays.asList(LABEL));
            expected.put(GROUP2, Arrays.asList(LABEL));
            validateMigrationList(jsonFile, expected);
        }

        @Test
        @Order(2)
        void testWildcardSingleSegment() {
            System.out.println("Running group wildcard single-segment tests.");
            List<String> command = new ArrayList<>();
            command.add(listScript);
            command.add(jsonFile.getPath());
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pwFile");
            command.add(passwordFile.getPath());
            command.add("--grp");
            command.add("Static*");

            Map<String, String> output = runProcess(command, 0);
            Map<String, List<String>> expected = new HashMap<>();
            expected.put(GROUP, Arrays.asList(LABEL));
            expected.put(GROUP2, Arrays.asList(LABEL));
            validateMigrationList(jsonFile, expected);
        }

        @Test
        @Order(3)
        void testExactMatch() {
            System.out.println("Running group exact match tests.");
            List<String> command = new ArrayList<>();
            command.add(listScript);
            command.add(jsonFile.getPath());
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pwFile");
            command.add(passwordFile.getPath());
            command.add("--grp");
            command.add("Static-Report-Migration-Test-2");

            Map<String, String> output = runProcess(command, 0);
            Map<String, List<String>> expected = new HashMap<>();
            expected.put(GROUP2, Arrays.asList(LABEL));
            validateMigrationList(jsonFile, expected);
        }

        @Test
        @Order(4)
        void testFileMatch() {
            System.out.println("Running group from file tests.");
            List<String> command = new ArrayList<>();
            command.add(listScript);
            command.add(jsonFile.getPath());
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pwFile");
            command.add(passwordFile.getPath());
            command.add("--grpf");
            command.add(new File(testDir, "samples/groups.txt").getPath());

            Map<String, String> output = runProcess(command, 0);
            Map<String, List<String>> expected  = new HashMap<>();
            expected.put(GROUP, Arrays.asList(LABEL));
            expected.put(GROUP2, Arrays.asList(LABEL));
            validateMigrationList(jsonFile, expected);
        }

        @Test
        @Order(5) // Execute last to prepare list for migration test
        void testWildcardMultiSegment() {
            System.out.println("Running group wildcard multi-segment tests.");
            List<String> command = new ArrayList<>();
            command.add(listScript);
            command.add(jsonFile.getPath());
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pwFile");
            command.add(passwordFile.getPath());
            command.add("--grp");
            command.add("*Static*Test");

            Map<String, String> output = runProcess(command, 0);
            Map<String, List<String>> expected = new HashMap<>();
            expected.put(GROUP, Arrays.asList(LABEL));
            validateMigrationList(jsonFile, expected);
        }
    }

    @Nested
    @Order(2)
    class MigrationsTests {
        @Test
        void migrationTest() {
            System.out.println("Running migration test.");
            // Assert json file has correct content from the previous test class
            Map<String, List<String>> expected = new HashMap<>();
            expected.put(GROUP, Arrays.asList(LABEL));
            validateMigrationList(jsonFile, expected);

            List<String> command = new ArrayList<>();
            command.add(migrateScript);
            command.add(jsonFile.getPath());
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pwFile");
            command.add(passwordFile.getPath());
            Map<String, String> output = runProcess(command, 0);
            assertTrue(output.get("err").trim().isEmpty(), String.format("Error stream is not empty\nOUT:\n%s\n\nERR:\n%s", output.get("out"), output.get("err")));
            validateResults();
        }
    }

    private static void setupCollection() throws Exception {
        System.out.println("Setting up collection.");
        client.setErrorStatusCode(maxDeleteError);
        client.deleteBuildResults(GROUP);
        client.deleteCollection(GROUP);
        client.deleteBuildResults(GROUP2);
        client.deleteCollection(GROUP2);
        client.setErrorStatusCode(maxNormError);
        
        client.createCollection(GROUP);
        BuildResult newResult = client.createBuildResult(GROUP, LABEL);
        newResult.setState(BuildResult.COMPLETE);

        String samplesFolder = "samples/";
        // Report data is labled with the version used to create it, in case of differences between versions
        newResult.setBuildReportData(new FileInputStream(new File(testDir, samplesFolder + "result-data-1.1.3.json")));
        newResult.setBuildReport(new FileInputStream(new File(testDir, samplesFolder + "report.html")));
        newResult.save();

        client.createCollection(GROUP2);
        newResult = client.createBuildResult(GROUP2, LABEL);
        newResult.setState(BuildResult.COMPLETE);

        // Report data is labled with the version used to create it, in case of differences between versions
        newResult.setBuildReportData(new FileInputStream(new File(testDir, samplesFolder + "result-data-1.1.3.json")));
        newResult.setBuildReport(new FileInputStream(new File(testDir, samplesFolder + "report.html")));
        newResult.save();

        // Assert file content
        System.out.println("Asserting test file content.");
        String metadataString = '{"date":"28-Feb-2022 17:26:26","build":"151","id":"DBB API Version","type":"VERSION","version":"1.1.3"}';
        List<BuildResult> results = client.getAllBuildResults(Collections.singletonMap(RepositoryClient.GROUP, GROUP));
        for (BuildResult result : results) {
            assertEquals(GROUP, result.getGroup());
            assertEquals(LABEL, result.getLabel());
            assertTrue(Utils.readFromStream(result.fetchBuildReport(), "UTF-8").contains(metadataString));
            assertTrue(Utils.readFromStream(result.fetchBuildReportData(), "UTF-8").contains(metadataString));
        }
        assertTrue(results.size() == 1);
    }

    private void validateMigrationList(File jsonFile, Map<String, List<String>> expected) {
        JSONObject json;
        try (BufferedReader reader = new BufferedReader(new FileReader(jsonFile))) {
            json = JSONObject.parse(reader);
        }
        
        expected.forEach((key, value) -> {
            assertTrue(json.containsKey(key), String.format("Group key '%s' not found in file.", key));
            assertIterableEquals(value, json.get(key), String.format("Results in group '%s' do not match: %s", key, json.get(key)));
        });
        assertTrue(json.size() == expected.size());
    }

    private void validateResults() {
        // Checks for script tags in the collections build report html
        System.out.println("Validating results.");
        for (BuildResult result : client.getAllBuildResults(Collections.singletonMap(RepositoryClient.GROUP, GROUP))) {
            String content = Utils.readFromStream(result.fetchBuildReport(), "UTF-8");
            assertTrue(content.contains('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en-us" lang="en-us" style="scroll-behavior: smooth;">'), String.format("Result data '%s:%s' not readable, bad encoding likely.", result.getGroup(), result.getLabel()));
            assertFalse(content.contains("</script>"), String.format("Result '%s:%s' not converted.", result.getGroup(), result.getLabel()));
        }
    }

    private Map<String, String> runProcess(List<String> command, int expectedRC) throws IOException, InterruptedException {
        ProcessBuilder processBuilder = new ProcessBuilder(command);
        processBuilder.environment().put("DBB_HOME", EnvVars.getHome());
        
        Process process = processBuilder.start();
        long startTime = System.currentTimeMillis();
        long maxTime = 3 * 60 * 1000; // Minutes (3) -> MS
        BufferedReader stdInput = new BufferedReader(new InputStreamReader(process.getInputStream()));
        BufferedWriter stdOut = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
        char[] buffer = new char[16*1024];

        StringBuilder output = new StringBuilder();
        while (System.currentTimeMillis() - startTime < maxTime) {
            int charsRead = stdInput.read(buffer);
            if (charsRead == -1) break;
            
            String newString = new String(buffer, 0, charsRead);
            output.append(newString);
            if (newString.toLowerCase().contains("('y' or 'n')")) {
                stdOut.write("y");
                stdOut.newLine();
                stdOut.flush();
            }
            
            Thread.sleep(1000);
        }

        StringBuilder error = new StringBuilder();
        BufferedReader stdError = new BufferedReader(new InputStreamReader(process.getErrorStream()));
        String line;
        while ((line = stdError.readLine()) != null) {
            error.append(line);
            error.append("\n");
        }
        if (process.isAlive()) {
            int timeLeft = maxTime - (System.currentTimeMillis() - startTime);
            if (timeLeft > 0) {
                if (!process.waitFor(timeLeft, TimeUnit.MILLISECONDS)) {
                    process.destroyForcibly();
                }
            } else {
                process.destroyForcibly();
            }
        }
        
        int rc = process.exitValue();
        String errorString = error.toString();
        String outputString = output.toString();

        assertEquals(expectedRC, rc, String.format("Script return code is not equal to %s\nOUT:\n%s\n\nERR:\n%s", expectedRC, outputString, errorString));
        
        Map<String, String> returnMap = new HashMap<>();
        returnMap.put("rc", rc);
        returnMap.put("out", outputString);
        returnMap.put("err", errorString);
        return returnMap;
    }
}