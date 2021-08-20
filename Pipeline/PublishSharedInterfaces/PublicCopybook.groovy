@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.repository.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import com.ibm.dbb.build.report.records.*
import com.ibm.dbb.build.report.*
import groovy.transform.*


// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def buildUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BuildUtilities.groovy"))
@Field RepositoryClient repositoryClient

println("** Building files mapped to ${this.class.getName()}.groovy script")

// verify required build properties
buildUtils.assertBuildProperties(props.cobol_requiredBuildProperties)

List<String> buildList = argMap.buildList

// reusing the cobol dataset definitions for copybooks
def langQualifier = "cobol"
buildUtils.createLanguageDatasets(langQualifier)

if (props.runzTests && props.runzTests.toBoolean()) {
    langQualifier = "cobol_test"
    buildUtils.createLanguageDatasets(langQualifier)
}

// iterate through build list
buildList.each { buildFile ->
    println "*** Building file $buildFile"

    // create mvs commands
    String member = CopyToPDS.createMemberName(buildFile)
    File logFile = new File( props.userBuild ? "${props.buildOutDir}/${member}.log" : "${props.buildOutDir}/${member}.copy.log")
    if (logFile.exists())
        logFile.delete()

    // compile the cobol program
    int rc = new CopyToPDS().file(new File(buildUtils.getAbsolutePath(buildFile))).dataset(props.cobol_cpyPDS).member(member).output(true).deployType("PublicCopy").execute()
    if (props.verbose) println "CopyToPDS for $buildFile = $rc"
    if (rc!=0){
    String errorMsg = "*! The compile return code ($rc) for $buildFile exceeded the maximum return code allowed ($maxRC)"
    println(errorMsg)
    props.error = "true"
    buildUtils.updateBuildResult(errorMsg:errorMsg,logs:["${member}.log":logFile],client:getRepositoryClient())
	}
}

// end script