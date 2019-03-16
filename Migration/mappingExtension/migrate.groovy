import com.ibm.dbb.migrate.*
import com.ibm.dbb.build.*
import java.io.File

/**********************************************************************************
usage: migrate [options] <data sets>|<mapping files>
Use this migration tool to migrate members from data sets to a local GIT
repository on HFS
 -m,--mapping <mapping>         The ID of mapping rule (optional), for
                                example:
                                com.ibm.dbb.migration.SimpleMapping,
                                com.ibm.dbb.migration.HlqMapping
 -o,--output <output>           Output of the generated mapping file
                                (optional)
 -p,--preview                   Perform a dry-run to generate a mapping
                                file (optional)
 -r,--repository <repository>   Local GIT repository to migrate to
                                (required)
***********************************************************************************/

def headerMsg = 'Use this migration tool to migrate members from data sets to a local GIT repository on HFS'
cli = new CliBuilder(usage:'migrate [options] <data sets>', header: headerMsg, stopAtNonOption: false)
cli.r(longOpt:'repository', args:1, argName:'repository', 'Local GIT repository to migrate to (required)')
cli.o(longOpt:'output', args:1, argName:'output', 'Output of the generated mapping file (optional)')
cli.m(longOpt:'mapping', args:1, argName:'mapping', 'The ID of mapping rule (optional), for example: com.ibm.dbb.migration.MappingRule')
cli.p(longOpt:'preview', 'Perform a dry-run to generate a mapping file (optional)')

def parameters = cli.parse(args)

//Validate that we have atleast an argument which is either a set of data sets to be imported or a mapping file
if (parameters.arguments().size() != 1)
{
    cli.usage()
    System.exit(2)
}
def arg = parameters.arguments()[0]
boolean isMappingFileSpecified = isMappingFileSpecified(arg)

def gitAttributePathCache = []
if (isMappingFileSpecified)
{        
    def repository = (parameters.r) ? new File(parameters.r) : null
    def gitAttributeWriter = null
    if (repository)
    {
        println("Local GIT repository: $repository")
        repository.mkdirs()
        gitAttributeWriter = new File(repository, ".gitattributes").newWriter(true)
    }
    
    def detector = new DetectPossibleRoundTripProblems()
    
    arg.split(",").each { path ->
        def mappingFile = new File(path)
        println("Migrate data sets using mapping file $mappingFile")
        mappingFile.eachLine { line ->
            //Ignore comment which starts with #
            if (!line.trim().startsWith("#"))
            {
                //Each line should be in the form of:  "dataset hfsPath pdsEncoding=Cp1047"
                def lineSegments = line.split(" ")
                if (lineSegments.size() >= 2)
                {
                    def datasetMember = lineSegments[0]
                    boolean isValidDatasetMember = datasetMember ==~ ".*\\((.*[a-zA-Z\\*].*)\\)"
                    if (isValidDatasetMember)
                    {
                        def datasetMemberSegment = datasetMember.split("[\\(\\)]")
                        def dataset = datasetMemberSegment[0]
                        def member = datasetMemberSegment[1]
                        def hfsPath = lineSegments[1];
                        def pdsEncoding = null;
                        if (lineSegments.size() > 2)
                        {
                            def pdsEncodingSegments = lineSegments[2].split("=")
                            if (pdsEncodingSegments.size() == 2 && pdsEncodingSegments[0] == 'pdsEncoding')
                                pdsEncoding = pdsEncodingSegments[1].trim()
                        }
                        def encodingString = pdsEncoding ?: 'default encoding'
                        println("Copying $datasetMemberSegment to $hfsPath using $encodingString")
                        
                        def (rc, msg) = detector(dataset, member)
                        if (rc)
                            println("Detecting possible migration in member $member.  Actual error is $msg")

                        new CopyToHFS().dataset(dataset).member(member).file(new File(hfsPath)).pdsEncoding(pdsEncoding).execute();
                        if (gitAttributeWriter)
                        {
                            String gitAttributeLine = null;
                            if (rc)
                                gitAttributeLine = repository.toPath().relativize(new File(hfsPath).toPath()).toFile().path + " binary"
                            else
                                gitAttributeLine = generateGitAttributeEncodingLine(repository, new File(hfsPath), gitAttributePathCache)
                            if (gitAttributeLine)
                                gitAttributeWriter.writeLine(gitAttributeLine)
                        }
                    }
                }
            }
        }
    }
    gitAttributeWriter?.close()
}
else
{
    def datasets = arg.split(",")
    
    //Verify that 'repository' option is specified
    if (!parameters.r)
    {
        cli.usage()
        System.exit(2)
    }
        
    //Create the repository directory if not exist
    def repository = new File(parameters.r)
    if (!repository.exists())
        repository.mkdirs()
    println("Local GIT repository: $repository")
    
    def isPreview = parameters.p
    if (isPreview)
        println("Preview flag is specified, no members will be copied to HFS")

    //Verify if mapping rule is defined and parse it, otherwise use default 'com.ibm.dbb.migration.SimpleMapping'
    def (mappingRuleId,mappingRuleAttrs) = parameters.m ? parseMappingRule(parameters.m) : ['com.ibm.dbb.migration.MappingRule',null]
    def mappingRule = (mappingRuleId as Class).newInstance(repository,mappingRuleAttrs)
    
    println("Using mapping rule $mappingRuleId to migrate the data sets")

    //Verify if an output file is specified to record the mapping
    def writer = null;
    if (parameters.o)
    {
        def outputFile = new File(parameters.o)
        writer = outputFile.newWriter(true)
        println("Generated mappings will be saved in $outputFile")
    }
    
    def gitAttributeWriter = !isPreview ? new File(repository, ".gitattributes").newWriter(true) : null 
    
    println("Start migration...")
    
    def detector = new DetectPossibleRoundTripProblems()
    
    datasets.each { dataset ->
        println("Migrating data set $dataset")
        def mappingInfos = mappingRule.generateMapping(dataset)
        mappingInfos.each { mappingInfo ->
            if (!isPreview)
            {                    
                def encodingString = mappingInfo.pdsEncoding ?: 'default encoding'
                println("Copying ${mappingInfo.getFullyQualifiedDsn()} to ${mappingInfo.hfsPath} using $encodingString")
                def (rc, msg) = detector(mappingInfo.dataset, mappingInfo.member)
                if (rc)
                    println("Detecting possible migration in member ${mappingInfo.member}.  Actual error is $msg")
                new CopyToHFS().dataset(mappingInfo.dataset).member(mappingInfo.member).file(new File(mappingInfo.hfsPath)).pdsEncoding(mappingInfo.pdsEncoding).execute();
                if (gitAttributeWriter)
                {
                    def hfsFile = new File(mappingInfo.hfsPath)
                    String gitAttributeLine = null
                    if (rc)
                        gitAttributeLine = repository.toPath().relativize(hfsFile.toPath()).toFile().path + " binary"
                    else
                        gitAttributeLine = generateGitAttributeEncodingLine(repository, new File(mappingInfo.hfsPath), gitAttributePathCache)
                    if (gitAttributeLine)
                        gitAttributeWriter.writeLine(gitAttributeLine)
                }                
            }    
            if (writer)
                writer.writeLine(mappingInfo.toString())
        }
    }

    writer?.close()
    gitAttributeWriter?.close()
}

def parseMappingRule(String mappingRuleId)
{  
    def mappingIds = ['MappingRule':'com.ibm.dbb.migration.MappingRule', 'com.ibm.dbb.migration.MappingRule':'com.ibm.dbb.migration.MappingRule']
    def temp = mappingRuleId.split("[\\[\\]]")
    if (temp.length == 1)
    {
        return [temp[0], [:]]
    }
    else if (temp.length == 2)
    {
        def id = temp[0]
        id = mappingIds[id]
        def str = temp[1]
        def attrMap = [:]
        def attrStrs = str.split(",")
        attrStrs.each { attrStr ->
            def attr = attrStr.split(":")
            def attrName = attr[0]
            def attrValue = attr[1]
            if (attrValue.startsWith("\"") && attrValue.endsWith("\"") && attrValue.length() > 2)
                attrValue = attrValue.substring(1,attrValue.length()-1)
            attrMap.put(attrName, attrValue)
        }
        return [id,attrMap]
    }
}

def generateGitAttributeEncodingLine(File root, File file, List<String> pathCache, String encoding = 'ibm-1047')
{
    def relPath = root.toPath().relativize(file.parentFile.toPath()).toFile()
    def index = file.name.lastIndexOf(".")
    def fileExtension = (index == -1 || index == (file.name.length() - 1)) ? null : file.name.substring(index+1)
    def extension = "*." + (fileExtension ?: "*")
    def path = relPath.path + '/' + extension
    if (pathCache.contains(path))
        return null
    pathCache.add(path)    
    return path + " working-tree-encoding=${encoding} git-encoding=utf-8"     
}

def isMappingFileSpecified(String argString)
{
    def filesExist = true
    argString.split(",").each { path ->
        def file = new File(path)
        if (!file.exists())
            filesExist = false
    }
    return filesExist
}
