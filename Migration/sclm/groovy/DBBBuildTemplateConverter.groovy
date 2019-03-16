/*
 * This script cannot be called as a standalone script since it requires
 * additional binding variable passed in from the parent script.
 * 
 * This script is used in conjunction with the BUILD.template to generate
 * the DBB build.groovy script.
 * 
 * @see GenerateBuildScrips.groovy.
 */

 
/*
 * buildXml represents the XML node <build> in DBB.xml file 
 */
def buildXml = getBinding().getVariable('buildXml')

/*
 * Generate the build order from the name of all <script> nodes
 * in DBB.xml.
 * 
 * @see ${DBBBuildTemplateConverter.getBuildOrder()} in BUILD.template
 */
def getBuildOrder()
{
    def scriptNames = []
    buildXml.scripts.script.each { script ->
        scriptNames << script.@name
    }    
    scriptNames.collect{"'" + it + "'"}.join(',')
}

/*
 * Generate a createPDS() call for each of the datasets in
 * DBB.xml file.
 * 
 * @see ${DBBBuildTemplateConverter.createDatasets()} in BUILD.template
 */
def createDatasets()
{
    def createPDS = []
    buildXml.datasets.dataset.each { dataset ->
        createPDS << "new CreatePDS().dataset(\"${dataset.@dsn}\").options(\"${dataset.@options}\").create()"
    }
    createPDS.collect{'    ' + it}.join('\n')
}

/*
 * Generate code to load property files. Handle both cases where
 * the property file is relative path vs absolute path.
 * 
 * @see ${DBBBuildTemplateConverter.loadPropertyFiles()} in BUILD.template
 */
def loadPropertyFiles()
{
    def outputs = []
    buildXml.propertyFiles.propertyFile.each { propertyFile ->
        if (propertyFile.@name) {        
            outputs << "properties.load(new File('${propertyFile.@name}.properties'))"
        }
        else if (propertyFile.@file) {        
            outputs << "properties.load(new File('${propertyFile.@file}'))"
        }
    }
    outputs.join('\n')
}

