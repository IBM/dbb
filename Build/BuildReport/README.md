# Build Report Sample
The following sample shows how to extend build report to provide additional functionality.

## Requirement
Record the Git commit hash value for each file being built in the build report.

## Background
DBB provides APIs to generate a build report to summarize what files being built. User can also use the APIs to extend the build report to add additional information. In this article, we will show you the steps to extend the build report APIs to provide git hash value of each file being built.

## Solution
We want to extend the build report to show additional column for displaying the commit hash value of each file

[[images/CustomBuildReport.png|Custom Build Report]]

We will use the sample Mortgage Application shipped with DBB to demonstrate the steps.  Here is the outline of the steps:
1. Creating GitHashRecord.java and GitRecordFactory.java
2. Retrieving and recording git hash value for each file in buildReport.json.
3. Registering GitRecordFactory.
3. Verifying GitHashRecord in buildReport.json.
4. Rendering Git hash in buildReport.html.

### Creating GitHashRecord.java and GitRecordFactory.java
The purpose of GitHashRecord is to store the Git hash value for each file.  The GitRecordFactory is used by DBB to create individual GitHashRecord that can be stored in the build report.
1. Create a Java project.
2. Set the classpath to include necessary DBB jar files.
3. Create a GitHashRecord.java that contains 2 fields: filePath and commitHashValue, and provide setters and getters for these 2 fields, see attachment GitHashRecord.java. 
4. Create a GitRecordFactory.java to create the GitHashRecord, see attachment GitRecordFactory.java.
5. Export the Java project as a jar file and FTP to the buildToolkit, and modify the classpath of the build accordingly.

### Retrieving and Recording Git Hash Value in BuildReport.json
1. The easy way to retrieve the Git commit hash value for each file is to use Git command "git ls-file".  Add the following sample code in the beginning of the build script:
 
>          //Generate a cache to store git hash for each file
>          def fileGitHashCache = [:]
>          def command = "git ls-files -s"
>          def commandOut = new StringBuffer()
>          def commandErr = new StringBuffer()
>          def process = command.execute(null, new File(properties.sourceDir))
>          process.waitForProcessOutput commandOut, commandErr
>          if (commandErr.toString().readLines().size() > 0)
>	            println("Error executing command $command: ${commandErr.toString()}")
>          else if (commandOut.toString().readLines().size() > 0)
>          {	
>             commandOut.toString().eachLine { line ->
>                 def (id, hash, eol, file) = line.tokenize()		
>                 fileGitHashCache[file] = hash 
>             }	
>          } 
>          def commandRC = process.exitValue()
 
2. As we compile each file, we will find the hash value and create a GitHashRecord to store this information in the build report.

>          buildFiles.each { file ->
>               run(new File(scriptName), [file] as String[])
>               processCounter++
>               def gitHashValue = fileGitHashCache[file]
>               def gitHashRecord = new GitHashRecord()
>               gitHashRecord.setFilePath(file)			
>               gitHashRecord.setCommitHashValue(gitHashValue)
>               BuildReportFactory.getBuildReport().addRecord(gitHashRecord)			 
>          }

### Registering GitRecordFactory
In order for DBB to record and save the record in buildReport.json, we need to register the new GitRecordFactory.  In the Jenkins project, add the following Java parameters:

>   -Ddbb.report.record.factories=com.ibm.team.dbb.build.ext.jenkins.report.JenkinsRecordFactory,com.ibm.team.dbb.build.ext.git.report.GitRecordFactory`

### Verifying GitHashRecord in buildReport.json
Request a build and go to the build result, select to view the build report data, you should be able to see the sample git hash record for each file being built.

>          "filePath":"MortgageApplication\/bms\/epsmlis.bms",
>          "commitHashValue":"2d9fd4aecbbab3a79b615b778bc48235d4272239",
>          "id":null,
>          "type":"GIT_HASH"

### Rendering Git hash in buildReport.html
For this step, we need to create a JavaScript that is similar to the default BuildSummaryRender.js.  Add the following code to render the Git Hash value stored in the buildReport.json.
1.  In the render() method, add the following script to store the git hash value to the record

>    else if (record.type == 'GIT_HASH')
>    {
>         cRecord.gitHash = record.commitHashValue 
>    }
    
2. In the renderExec(), add the following script to render the git hash value as HTML

>    rowHtml += "<td rowspan='" + cRecord.executors.length + "'>" + cRecord.gitHash + "</td>";

See attachment BuildSummaryRender.js.

## Attachment
* GitHashRecord.java - to store Git commit hash value.
* GitRecordFactory.java - to create and parse the record containing Git commit hash value.
* BuildSummaryRender.js - to render the Git commit hash value in HTML.
* BuildReport.html - load the BuildSummaryRender.js.  
* DefaultTheme.css - CSS styles used by the BuildReport.html.



