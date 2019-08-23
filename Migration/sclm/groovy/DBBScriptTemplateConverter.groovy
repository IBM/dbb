/*
 * This script cannot be called as a standalone script since it requires
 * additional binding variable passed in from the parent script.
 *
 * This script is used in conjunction with the SCRIPT.template to generate
 * the DBB script.groovy script.
 *
 * @see GenerateBuildScrips.groovy.
 */
import groovy.transform.Field
import com.ibm.dbb.build.*

/*
 * This scriptXml represents the <script> node in DBB.xml
 */
def scriptXml = getBinding().getVariable('scriptXml')

/*
 * The File object of the DBB.xml file
 */
def dbbXmlFile = getBinding().getVariable('dbbXmlFile')

/*
 * The location of the generated output groovy scripts
 */
def outputDir = getBinding().getVariable('outputDir')

/*
 * A map to store all variables per Executable.
 * We need this map when generate the body of each
 * Executable and the invocation of this Executable.
 */
@Field def execVarsMap = [:]

/*
 * We only want to generate the execVarsMap once per script.
 */
@Field def isInit = false

def init()
{
    if (!isInit)
    {
        /*
         * Find all the ${} variables needed per Executable.
         */
        scriptXml.execute.each { execute ->

            def candidates = []
            execute.'**'.each { child ->
                candidates += child.attributes().values().findAll { value ->
                    value.contains('${')
                }
            }
            candidates.unique()
            def vars = []
            candidates.each { candidate ->
                vars += candidate.findAll(/\$\{([^\$\{\}]+)+\}/) { match, group ->
                    group
                }
            }
            vars.unique()

            def execVars = []
            execute.property.each { property ->
                execVars << property.@name
            }

            vars -= execVars

            execVarsMap."${execute.@name}" = vars
        }
        //println execVarsMap
        isInit = true
    }
}

/*
 * Generate the body of an Executable.
 *
 * @see ${DBBScriptTemplateConverter.generateExecuteMethods()} in SCRIPT.template
 */
def generateExecuteMethods()
{
    def outputs = []
    scriptXml.execute.each { execute ->
        def execOutput = []

        /*
         * The name of the Executable could contain invalid characters when
         * used for the name of the Groovy method.  Convert all invalid
         * characters to '_'
         */
        def executeName = convertToJavaIdentifier(execute.@name)
        execOutput << "def ${executeName}(Map args) {"
        def vars = execVarsMap."${execute.@name}"
        vars.each { var ->
            execOutput << "    def ${var} = args.${var}"
        }

        execute.property.each { property ->
            if (property.@name.startsWith('DBB_CONDITIONAL_')) {
                execOutput << "    final def ${property.@name} = ${property.@value}"
            }
            else {
                execOutput << "    final def CONST_${property.@name} = \"${property.@value}\""
                execOutput << "    def ${property.@name} = BuildProperties.getFileProperty(\"${property.@name}\", FILE) ?: CONST_${property.@name}"
            }
        }

        /* Make the generated script looking nicer if no variables */
        if (vars.size() > 0 || execute.property.size() > 0)
            execOutput << ''

        if (execute.@type == 'mvs' || execute.@type == 'tso' || execute.@type == 'ispf')
        {
            def tempOutput = ''
            def methodName = executeName
            if (execute.@type == 'mvs')
            {
                tempOutput += "    def $methodName = new MVSExec()"
                tempOutput += execute.@file ? ".file(\"${execute.@file}\")" : ''
                tempOutput += execute.@pgm ? ".pgm(\"${execute.@pgm}\")" : ''
                tempOutput += execute.@ddnames ? ".ddnames(\"${execute.@ddnames}\")" : ''
                if (execute.@parm) {
                    //handle parm variable substitution
                    def parms = []
                    execute.@parm.split(',').each {
                        def segment = it.trim()
                        if (segment.startsWith('SSI')) {
                            //SSI_INFO is specific to RTC, ignore for now
                        }
                        /*
                        else if (segment.contains('@{ssi_info}')) {
                            def ssiInfo = String.format("%08x", System.currentTimeMillis()).toUpperCase()
                            segment = segment.replace('@{ssi_info}', ssiInfo)
                            parms << segment
                        }
                        */
                        else {
                            //handle param contains variable
                            def varName = segment.startsWith('&amp;') ? segment.drop(5) : segment
                            varName = varName.startsWith('&') ? varName.drop(1) : varName
                            parms << (varName.size() == segment.size() ? segment : '${'+varName+'}')
                        }
                    }
                    tempOutput += ".parm(\"${parms.join(',')}\")"
                }
                tempOutput += execute.@freePgmAllocatedDDs ? ".freePgmAllocatedDDs(\"${execute.@freePgmAllocatedDDs}\")" : ''
            }
            else
            {
                tempOutput += "    def $methodName = "
                tempOutput += (execute.@type == 'tso') ? 'new TSOExec()' : 'new ISPFExec()'
                tempOutput += execute.@file ? ".file(\"${execute.@file}\")" : ''
                tempOutput += execute.@command ? ".command(\"${execute.@command}\")" : ''
                tempOutput += execute.@options ? ".options(\"${execute.@options}\")" : ''
                tempOutput += execute.@confDir ? ".confDir(\"${execute.@confDir}\")" : ''
                tempOutput += execute.@logLevel ? ".logLevel(\"${execute.@logLevel}\")" : ''
                tempOutput += execute.@logFile ? ".logFile(\"${execute.@logFile}\")" : ''
                tempOutput += execute.@logEncoding ? ".logEncoding(\"${execute.@logEncoding}\")" : ''
                tempOutput += execute.@reuseIspfSession ? ".reuseIspfSession(\"${execute.@reuseIspfSession}\")" : ''
                tempOutput += execute.@keepCommandScript ? ".keepCommandScript(\"${execute.@keepCommandScript}\")" : ''
                tempOutput += execute.@response ? ".response(\"${execute.@response}\")" : ''
                tempOutput += execute.@shExec ? ".shExec(\"${execute.@shExec}\")" : ''
                tempOutput += execute.@gatewayType ? ".gatewayType(\"${execute.@gatewayType}\")" : ''
                tempOutput += execute.@procedureName ? ".procedureName(\"${execute.@procedureName}\")" : ''
                tempOutput += execute.@accountNumber ? ".accountNumber(\"${execute.@accountNumber}\")" : ''
                tempOutput += execute.@groupId ? ".groupId(\"${execute.@groupId}\")" : ''
                tempOutput += execute.@regionSize ? ".regionSize(\"${execute.@regionSize}\")" : ''
            }
            execOutput << tempOutput

            execute.dd.each { dd ->
                if (dd.@condition) {
                    execOutput << "    if (${dd.@condition}) {"
                }
                execOutput << (dd.@condition ? "    " : "") + convertDDToDDStatement(dd, methodName)
                dd.dd.each { subdd ->
                    execOutput << convertDDToDDStatement(subdd, methodName)
                }
                if (dd.@condition) {
                    execOutput << '    }'
                }
            }
            execOutput << "    ${methodName}.execute()"
        }
        else if (execute.@type == 'jcl')
        {
            def tempOutput = "    def $methodName = new JCLExec()"
            tempOutput += execute.@dataset ? ".dataset(\"${execute.@dataset}\")" : ''
            tempOutput += execute.@member ? ".member(\"${execute.@member}\")" : ''
            tempOutput += execute.@file ? ".file(\"${execute.@file}\")" : ''
            tempOutput += execute.@text ? ".text(\"${execute.@text}\")" : ''
            tempOutput += execute.@jclEncoding ? ".jclEncoding(\"${execute.@jclEncoding}\")" : ''
            tempOutput += execute.@confDir ? ".confDir(\"${execute.@confDir}\")" : ''
            tempOutput += execute.volser ? ".volser(\"${execute.@volser}\")" : ''

            execOutput << tempOutput
            execOutput << "     ${methodName}.execute()"

        }

        execOutput << '}'
        outputs << execOutput.join('\n')
    }

    outputs.join('\n\n')
}

/*
 * Generate the invocation of each Executable.  Need to
 * pass in the correct required variables.
 *
 * @see ${DBBScriptTemplateConverter.generateExecuteMethodInvocations()} in SCRIPT.groovy
 */
def generateExecuteMethodInvocations()
{
    def outputs = []
    scriptXml.execute.each { execute ->
        def executeName = convertToJavaIdentifier(execute.@name)
        def invocationOutput = "rc = ${executeName}("
        def vars = execVarsMap."${execute.@name}"
        invocationOutput += vars?.collect {"$it : $it"}.join(', ')
        invocationOutput += ")"
        outputs << invocationOutput

        def maxRC = execute.@maxRC ?: 0
        if (maxRC)
            outputs << "if (failOnError && rc > $maxRC)"
        else
            outputs << "if (failOnError && rc)"
        outputs << '{' << "    println \"${executeName} failed, the return code (" + '$rc' + ") is greater than maxRC (${maxRC})\""
        outputs << "    System.exit(1)" << "}" << ""
    }
    outputs.join('\n')
}

/*
 * Generate the resolution rule for resolving dependencies. The logic is:
 * 1. Go through all DDAllocation and DDConcatenation and group them by DDName.
 * 2. For each of the collected DD in step 1, make sure they start with {HLQ} and
 *    can be mapped to a folder using the information in datasetMappings.properties.
 * 3. Generate the Groovy script based on the information collected in step 2.
 *
 * @see ${DBBScriptTemplateConverter.generateResolutionRules()} in SCRIPT.template
 */
def generateResolutionRules()
{
    outputs = []
    def datasetMappingsFile = new File(outputDir, 'datasetMappings.properties')
    def properties = BuildProperties.getInstance()
    properties.load(datasetMappingsFile)

    scriptXml.execute.each { execute ->
        concats = [:]
        def prevDDName
        execute.dd.each { dd ->

            if (dd.@dsn) {
                if (dd.@name) {
                    def ddnameKey = convertToJavaIdentifier(dd.@name)
                    def dds = concats."$ddnameKey"
                    if (!dds) {
                        dds = [] as Set
                        concats."$ddnameKey" = dds
                    }
                    dds << dd

                    dd.dd.each { subdd ->
                        dds << subdd
                    }
                }
            }
        }

        def ddResRuleMap = [:]
        concats.each { name,dds ->
            def paths = [] as Set
            dds.each { dd ->
                def fullyQualifiedDsn = dd.@dsn.toUpperCase()
                def dsn = fullyQualifiedDsn - '${HLQ}.'
                dsn = dsn.take(dsn.indexOf('(')) ?: dsn
                def patterns = properties.getFilePropertyPatterns('datasetMapping', dsn)
                patterns.each { pattern ->
                    paths << pattern
                }
            }
            ddResRuleMap."$name" = paths
        }

        ddResRuleMap.each {
            def ddName = it.key
            def paths = it.value
            def varName = (scriptXml.execute.size() > 1 ? "${execute.@name}_$ddName" : ddName) + "_rule"
            if (paths)
            {
                outputs << "def $varName = new ResolutionRule().library('$ddName')"
                paths.each { path ->
                    outputs << "${varName}.addPath(collection, sourceDir, '$path')"
                }
                outputs << "resolver.addResolutionRule($varName)"
            }
        }

        outputs << ""
    }

    outputs.join('\n')
}

/*
 * Utility method to convert an executor name into
 * a valid Java/Groovy method name.
 */
def convertToJavaIdentifier(def text)
{
    def newText = ''
    text.getChars().eachWithIndex { ch, index ->
        boolean isValid = (index == 0 ? Character.isJavaIdentifierStart(ch) : Character.isJavaIdentifierPart(ch))
        newText += (isValid ? ch : '_')
    }
    newText
}

def convertDDToDDStatement(def dd, def methodName)
{
    def ddOutput = ''
    if (dd.@dsn || dd.@name) {
        ddOutput += "    ${methodName}.dd(new DDStatement()"
        ddOutput += dd.@name ? ".name(\"${dd.@name}\")" : ""
        ddOutput += dd.@dsn ? ".dsn(\"${dd.@dsn}\")" : ""
        ddOutput += dd.@options ? ".options(\"${dd.@options}\")" : ""
        ddOutput += dd.@report ? ".report(${Boolean.valueOf(dd.@report)})" : ""
        ddOutput += dd.@output ? ".output(${Boolean.valueOf(dd.@output)})" : ""
        ddOutput += dd.@pass ? ".pass(${Boolean.valueOf(dd.@pass)})" : ""
        ddOutput += dd.@deployType ? ".deployType(\"${dd.@deployType}\")" : ""
        ddOutput += dd.@instreamData ? ".instreamData(\"${dd.@instreamData}\")" : ""
        ddOutput += ')'
    }
    ddOutput
}
