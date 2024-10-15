@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import com.ibm.dbb.metadata.*
import com.ibm.dbb.dependency.*
import com.ibm.dbb.build.*
import groovy.transform.*
import com.ibm.dbb.build.report.*
import com.ibm.dbb.build.report.records.*
import groovy.yaml.YamlSlurper
import groovy.yaml.YamlBuilder
import groovy.lang.GroovyShell
import groovy.util.*
import java.nio.file.*

/**
 * Utilities to read, update or export existing ApplicationDescriptor from/to YAML 
 */

class ApplicationDescriptor {
    String application
    String description
    String owner
    ArrayList<Source> sources
    ArrayList<Baseline> baselines
    ArrayList<DependencyDescriptor> dependencies
    ArrayList<String> consumers
}

class Source {
    String name
    String repositoryPath
    String language
    String languageProcessor
    String fileExtension
    String artifactsType
    ArrayList<FileDef> files
}

class FileDef {
    String name
    String type
    String usage
}

class Baseline {
    String branch
    String baseline
}

class DependencyDescriptor {
    String name
    String version
    String type
}

/**
 * 
 * Reads an existing application descriptor YAML
 * returns an ApplicationDescriptor Object
 * 
 */
def readApplicationDescriptor(File yamlFile) {
    // Internal objects
    def yamlSlurper = new groovy.yaml.YamlSlurper()
    ApplicationDescriptor applicationDescriptor = yamlSlurper.parse(yamlFile)
    return applicationDescriptor
}

/**
 * Write an ApplicationDescriptor Object into a YAML file
 */
def writeApplicationDescriptor(File yamlFile, ApplicationDescriptor applicationDescriptor) {
    // Sort source groups and files by name before writing to YAML file
    if (applicationDescriptor.sources) {
        applicationDescriptor.sources.sort {
            it.name
        }
        applicationDescriptor.sources.each() { source ->
            source.files.sort {
                it.name
            }
        }
    }

    def yamlBuilder = new YamlBuilder()
    // build updated application descriptor

    yamlBuilder {
        application applicationDescriptor.application
        description applicationDescriptor.description
        owner applicationDescriptor.owner
        sources (applicationDescriptor.sources)
        baselines (applicationDescriptor.baselines)
        if (applicationDescriptor.dependencies) {
            dependencies applicationDescriptor.dependencies
        }
        if (applicationDescriptor.consumers) {
            consumers applicationDescriptor.consumers
        }
    }

    // write file
    yamlFile.withWriter("IBM-1047") { writer ->
        writer.write(yamlBuilder.toString())
    }
	Process process = "chtag -tc IBM-1047 ${yamlFile.getAbsolutePath()}".execute()
	process.waitFor()   
}

/**
 * Method to update the Application Descriptor
 * 
 * Appends to an existing source sourceGroupName, if it exists.
 * If the sourceGroupName cannot be found, it creates a new sourceGroup
 * 
 */

def appendFileDefinition(ApplicationDescriptor applicationDescriptor, String sourceGroupName,
							String language, String languageProcessor, String artifactsType,
							String fileExtension, String repositoryPath, String name,
							String type, String usage) {

    def sourceGroupRecord

    def fileRecord = new FileDef()
    fileRecord.name = name
    fileRecord.type = type
    fileRecord.usage = usage
    
    if (!applicationDescriptor.sources) {
       applicationDescriptor.sources = new ArrayList<Source>()
    }

    existingSourceGroup = applicationDescriptor.sources.find(){ source ->
        source.name == sourceGroupName
    }

    if (existingSourceGroup) { // append file record definition to existing sourceGroup
        sourceGroupRecord = existingSourceGroup
        
        // check if the fileRecord already exists, and this is an update

        existingFileRecord = sourceGroupRecord.files.find(){ file ->
            file.name == fileRecord.name
        }

        if (existingFileRecord) { // update existing file record
            existingFileRecord.type = type
            existingFileRecord.usage = usage
		} else { // add a new record
            sourceGroupRecord.files.add(fileRecord)
        }
	} else {
        // create a new source group entry
        sourceGroupRecord = new Source()
        sourceGroupRecord.name = sourceGroupName
        sourceGroupRecord.language = language
        sourceGroupRecord.languageProcessor = languageProcessor
        sourceGroupRecord.fileExtension = fileExtension
        sourceGroupRecord.artifactsType = artifactsType
        sourceGroupRecord.repositoryPath = repositoryPath

        sourceGroupRecord.files = new ArrayList<FileDef>()
        // append file record
        sourceGroupRecord.files.add(fileRecord)
        applicationDescriptor.sources.add(sourceGroupRecord)
    }
}


/**
 * Method to remove a file from the Application Descriptor
 */

def removeFileDefinition(ApplicationDescriptor applicationDescriptor, String sourceGroupName, String name) {

    if (applicationDescriptor.sources) {
        def existingSourceGroup = applicationDescriptor.sources.find() { source ->
            source.name == sourceGroupName
        }
        if (existingSourceGroup) { // Found an existing Source Group that matches
            def existingFileDef = existingSourceGroup.files.find { file ->
                file.name.equals(name)
            }
            if (existingFileDef) {
//                println "Found matching file ${existingFileDef.name}"
                existingSourceGroup.files.remove(existingFileDef)
            }
        }
    }
}

/**
 * Method to add an application dependency 
 */

def addApplicationDependency(ApplicationDescriptor applicationDescriptor, String applicationDependency, String version, String type) {
    if (!applicationDescriptor.dependencies) {
        applicationDescriptor.dependencies = new ArrayList<DependencyDescriptor>()
    }
    def existingDependencies = applicationDescriptor.dependencies.findAll() {
        it.name.equals(applicationDependency) & it.type.equals(type)
    }
    if (!existingDependencies) {
        def dependency = new DependencyDescriptor()
        dependency.name = applicationDependency
        dependency.version = version
        dependency.type = type
        applicationDescriptor.dependencies.add(dependency)
        applicationDescriptor.dependencies.sort {
            it.name
        } 
    }
}

/**
 * Method to add a consumer to list of consumers
 */

def addApplicationConsumer(ApplicationDescriptor applicationDescriptor, String consumingApplication) {
    
    if (!applicationDescriptor.consumers) {
        applicationDescriptor.consumers = new ArrayList<String>()
    }
    // don't add the "owning" application
    if (applicationDescriptor.application != consumingApplication) {
        def existingConsumers = applicationDescriptor.consumers.findAll() {
            it.equals(consumingApplication)
        }
        if (!existingConsumers) {     
            applicationDescriptor.consumers.add(consumingApplication)
            applicationDescriptor.consumers.sort()
        }
    } 
}

/**
 * Method to drop the source entries
 */

def resetAllSourceGroups(ApplicationDescriptor applicationDescriptor) {
    applicationDescriptor.sources = new ArrayList<Source>()
}

/**
 * Method to drop the source entries
 */

def resetConsumersAndDependencies(ApplicationDescriptor applicationDescriptor) {
    applicationDescriptor.consumers = new ArrayList<Source>()
    applicationDescriptor.dependencies = new ArrayList<Source>()
}

/**
 * Method to create an empty application descriptor object 
 */
def createEmptyApplicationDescriptor() {
    ApplicationDescriptor applicationDescriptor = new ApplicationDescriptor()
    applicationDescriptor.sources = new ArrayList<Source>()
    applicationDescriptor.baselines = new ArrayList<Baseline>()
    return applicationDescriptor
}

/**
 * Method to return a FileDef that matches what we are looking for
 * Return null if nothing is found
 * Return null if multiple entries are found (for source groups or files) with a warning message
 */

def getFileUsage(ApplicationDescriptor applicationDescriptor, String sourceGroup, String name) {
	if (applicationDescriptor) {
		def matchingSourceGroups = applicationDescriptor.sources.findAll() { matchingSourceGroup ->
			matchingSourceGroup.name.equals(sourceGroup)
		}
		if (matchingSourceGroups) {
			if (matchingSourceGroups.size() == 1) {
				def matchingFiles = matchingSourceGroups[0].files.findAll() { matchingFile ->
					matchingFile.name.equalsIgnoreCase(name)
				}
				if (matchingFiles) {
					if (matchingFiles.size() == 1) {
						return matchingFiles[0].usage
					} else {
						println("*! [WARNING] Multiple Files found matching '${name}'. Skipping search.")
						return null
					}
				} else {
					return null
				}
			} else {
				println("*! [WARNING] Multiple Source Groups found matching '${sourceGroup}'. Skipping search.")
				return null
			}
		} else {
			return null
		}
	} else {
		return null
	}
}

/**
 * Method to return an ArraList that contains relative Paths
 * to public & shared include files
 */

def getFilesByTypeAndUsage(ApplicationDescriptor applicationDescriptor, String artifactsType, String usage) {
	if (applicationDescriptor) {
		def matchingSourceGroups = applicationDescriptor.sources.findAll() { matchingSourceGroup ->
			matchingSourceGroup.artifactsType.equals(artifactsType)
		}
		ArrayList<String> files = new ArrayList<String>()

		if (matchingSourceGroups) {
			matchingSourceGroups.each() { matchingSourceGroup ->
				def matchingFiles = matchingSourceGroup.files.findAll() { matchingFile ->
					matchingFile.usage.equals(usage)
				}
				if (matchingFiles) {
					matchingFiles.each() { matchingFile ->
						files.add("${matchingSourceGroup.repositoryPath}/${matchingFile.name}.${matchingSourceGroup.fileExtension}")
					}
				}
			}
		}
		if (!files.isEmpty()) {
			return files
		}
	}
}

/**
 * Method to add a baseline 
 * If an existing baseline for a given branch already exists, the method replaces it
 */

def addBaseline(ApplicationDescriptor applicationDescriptor, String branch, String baseline) {
	if (applicationDescriptor.baselines) {
		def existingBaselines = applicationDescriptor.baselines.findAll() { baselineDefinition ->
			baselineDefinition.branch.equals(branch)
		}
		existingBaselines.forEach() { existingBaseline ->
			applicationDescriptor.baselines.remove(existingBaseline)
	    }
	} else {
		applicationDescriptor.baselines = new ArrayList<Baseline>()
	}

	if (applicationDescriptor.baselines) {
		applicationDescriptor.sources = new ArrayList<Source>()
	}
	Baseline newBaseline = new Baseline()
	newBaseline.branch = branch
	newBaseline.baseline = baseline
	applicationDescriptor.baselines.add(newBaseline)
}
