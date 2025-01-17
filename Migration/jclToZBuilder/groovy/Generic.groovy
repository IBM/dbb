@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.metadata.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.yaml.YamlSlurper
import groovy.lang.GroovyShell
import groovy.util.*


// define script properties
@Field BuildProperties props = BuildProperties.getInstance()
@Field def buildUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BuildUtilities.groovy"))
@Field def impactUtils= loadScript(new File("${props.zAppBuildDir}/utilities/ImpactUtilities.groovy"))
@Field def bindUtils= loadScript(new File("${props.zAppBuildDir}/utilities/BindUtilities.groovy"))
	
println("** Building ${argMap.buildList.size()} ${argMap.buildList.size() == 1 ? 'file' : 'files'} mapped to ${this.class.getName()}.groovy script")

def yamlSlurper = new groovy.yaml.YamlSlurper()

int currentBuildFileNumber = 1

// iterate through build list
argMap.buildList.each { buildFile ->
	println "*** (${currentBuildFileNumber++}/${argMap.buildList.size()}) Building file $buildFile"

    String buildFileName = (new File(buildFile)).getName();

    def CorrespondingConfigurationFile = "CBSA/Configuration/${buildFileName}.yaml"
    println "*** Corresponding Configuration file is $CorrespondingConfigurationFile"

    def YAMLMap = yamlSlurper.parse(new File(CorrespondingConfigurationFile))

	MVSJob job = new MVSJob()
	job.start()

    YAMLMap.steps.each { step ->
        def stepExecution = forgeStep(step, buildFile)
        int rc = stepExecution.execute();    
        println("*** Return code for step ${step.program}: ${rc}")
    }

    job.stop()
}


def forgeStep(step, buildFile) {
    def program = step.program
    def parms = step.parms
    MVSExec stepExecution = new MVSExec().file(buildFile).pgm(program).parm(parms)
    step.DDs.each { DD ->
        for (int indexDSN = 0; indexDSN < DD.DSNs.size(); indexDSN++) {
            if (indexDSN == 0) {
                if (DD.DSNs[indexDSN].output == null) {
                    stepExecution.dd(new DDStatement().name(DD.name).dsn(DD.DSNs[indexDSN].dsn).options(DD.DSNs[indexDSN].options))
                } else {
                    stepExecution.dd(new DDStatement().name(DD.name).dsn(DD.DSNs[indexDSN].dsn).options(DD.DSNs[indexDSN].options).output(true).deployType(DD.DSNs[indexDSN].output))
                }
            } else {
                if (DD.DSNs[indexDSN].output == null) {
                    stepExecution.dd(new DDStatement().dsn(DD.DSNs[indexDSN].dsn).options(DD.DSNs[indexDSN].options))
                } else {
                    stepExecution.dd(new DDStatement().dsn(DD.DSNs[indexDSN].dsn).options(DD.DSNs[indexDSN].options).output(true).deployType(DD.DSNs[indexDSN].output))
                }
            }
        }
        if (DD.copy) {
            String buildFileName = (new File(buildFile)).getName();
            String logFile = "${props.buildOutDir}/${buildFileName}-${program}-${DD.name}.log"
            stepExecution.copy(new CopyToHFS().ddName(DD.name).file(new File(logFile)).hfsEncoding(DD.encoding))
        }
    }
    return stepExecution
}
