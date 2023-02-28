import com.ibm.dbb.repository.RepositoryClient;
import com.ibm.dbb.repository.BuildResult;
import com.ibm.dbb.EnvVars;
import com.ibm.dbb.build.internal.Utils;

import java.util.concurrent.TimeUnit;
import java.lang.Thread;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;

import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;


class StaticReportMigrationTests {
    private static final String GROUP = "Static-Report-Migration-Test";
    private static final String LABEL = "buildresult";
    private static final String URL_KEY = "test-url";
    private static final String ID_KEY = "test-id";
    private static final String PW_FILE_KEY = "test-pwFile";

    private static File testDir = new File(StaticReportMigrationTests.getProtectionDomain().getCodeSource().getLocation().getPath()).getParentFile();
    private static String script = new File(testDir, "../bin/static-report-migration.sh").getPath();

    private static String url;
    private static String id;
    private static File passwordFile;
    private static RepositoryClient client;

    private static final int maxNormError = 202;
    private static final int maxDeleteError = 405;

    @Nested
    @TestInstance(Lifecycle.PER_CLASS)
    class IntegrationTests {
        @BeforeAll
        static void setupCollection() throws Exception {
            System.out.println("Setting up collection.");
            client.setErrorStatusCode(maxDeleteError);
            client.deleteBuildResults(GROUP);
            client.deleteCollection(GROUP);
            client.setErrorStatusCode(maxNormError);
            
            client.createCollection(GROUP);
            BuildResult newResult = client.createBuildResult(GROUP, LABEL);
            newResult.setState(BuildResult.COMPLETE);

            String samplesFolder = "samples/";
            // Report data is labled with the version used to create it, in case of differences between versions
            newResult.setBuildReportData(new FileInputStream(new File(testDir, samplesFolder + "result-data-1.1.3.json")));
            newResult.setBuildReport(new FileInputStream(new File(testDir, samplesFolder + "report.html")));
            newResult.save();

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

        @Test
        void migrationTest() {
            System.out.println("Running migration test.");
            List<String> command = new ArrayList<>();
            command.add(script);
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pwFile");
            command.add(passwordFile.getPath());
            command.add("--grp");
            command.add(GROUP);
            Map<String, String> output = runMigrationScript(command, 0);
            assertTrue(output.get("err").trim().isEmpty(), String.format("Error stream is not empty\nOUT:\n%s\n\nERR:\n%s", output.get("out"), output.get("err")));
            validateResults();
        }

        @Test
        void badPasswordFileTest() {
            System.out.println("Running bad password file test.");
            List<String> command = new ArrayList<>();
            command.add(script);
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pwFile");
            command.add("~/nonexistantfile");
            command.add("--grp");
            command.add(GROUP);
            Map<String, String> output = runMigrationScript(command, 2);
            assertTrue(output.get("out").contains("There was an issue reading your password file"));
        }

        @Test
        void badPasswordTest() {
            System.out.println("Running bad password test.");
            List<String> command = new ArrayList<>();
            command.add(script);
            command.add("--url");
            command.add(url);
            command.add("--id");
            command.add(id);
            command.add("--pw");
            command.add("BADPASSWORD");
            command.add("--grp");
            command.add(GROUP);
            Map<String, String> output = runMigrationScript(command, 2);
            assertTrue(output.get("out").contains("There was an issue connecting to the Repository Client"));
        }
    }

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
    }

    @AfterAll
    static void cleanupStore() {
        System.out.println("Cleaning up store.");
        client.setErrorStatusCode(maxDeleteError);
        client.deleteBuildResults(GROUP);
        client.deleteCollection(GROUP);
    }

    private void validateResults() {
        // Checks for script tags in the collections build report html
        System.out.println("Validating results.");
        for (BuildResult result : client.getAllBuildResults(Collections.singletonMap(RepositoryClient.GROUP, GROUP))) {
            String content = Utils.readFromStream(result.fetchBuildReport(), "UTF-8");
            assertTrue(content.contains('<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en-us" lang="en-us">'), String.format("Result data '%s%s' not readable, bad encoding likely.", result.getGroup(), result.getLabel()));
            assertFalse(content.contains("</script>"), String.format("Result '%s:%s' not converted.", result.getGroup(), result.getLabel()));
        }
    }

    private Map<String, String> runMigrationScript(String command, int expectedRC) throws IOException, InterruptedException {
        return runMigrationScript(Arrays.asList(command.split(" ")), expectedRC);
    }

    private Map<String, String> runMigrationScript(List<String> command, int expectedRC) throws IOException, InterruptedException {
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
        process.destroy();
        
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