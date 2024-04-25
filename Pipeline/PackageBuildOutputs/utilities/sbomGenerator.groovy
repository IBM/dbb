@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import java.io.File
import com.ibm.dbb.build.CopyToHFS
import com.ibm.dbb.build.BuildProperties
import com.ibm.dbb.build.DBBConstants
import com.ibm.dbb.build.DBBConstants.CopyMode
import com.ibm.dbb.build.report.BuildReport
import com.ibm.dbb.build.report.records.*
import groovy.transform.*
import groovy.cli.commons.*
import java.nio.file.*
import static java.nio.file.StandardCopyOption.*
import com.ibm.jzos.ZFile
import org.cyclonedx.*
import org.cyclonedx.model.*

/************************************************************************************
 * This utility script helps generating an SBM from the DBB BuildReport
 *
 * Version 1 - 04/2024
 *  Initial implementation of SBOM Generation
 ************************************************************************************/


// Property prefix, matching what is defined in zAppBuild/build.groovy
@Field String hashPrefix = ':githash:'
@Field bomRef = 0

// SBOM generation-related objects
@Field HashSet<String> applications
@Field HashMap<String, HashMap<String, String>> outputsInSBOM
@Field ArrayList<Dependency> sbomDependencies
@Field Bom sbom

def initializeSBOM(String sbomAuthor) {
	// HashMap of files to report in SBOM
	applications = new HashSet<String>()
	outputsInSBOM = new HashMap<String, HashMap<String, String>>()
	sbom = new Bom();
	sbom.setSerialNumber("url:uuid:" + UUID.randomUUID().toString());
	sbom.setVersion(1);
	LifecycleChoice sbomLifecycleChoice = new LifecycleChoice()
	sbomLifecycleChoice.setPhase(LifecycleChoice.Phase.POST_BUILD)
	Lifecycles sbomLifecycles = new Lifecycles()
	sbomLifecycles.setLifecycleChoice([sbomLifecycleChoice])
	Metadata sbomMetadata = new Metadata()
	sbomMetadata.setLifecycles(sbomLifecycles)
	// Add the Author is passed on command-line, and well-formed ("Name <email>" expected)
	def sbomAuthorFields = sbomAuthor.split('<')
	if (sbomAuthorFields.size() == 2) {
		OrganizationalContact author = new OrganizationalContact()
		author.setName(sbomAuthorFields[0].trim())
		author.setEmail(sbomAuthorFields[1].replaceAll(">", "").trim())
		sbomMetadata.addAuthor(author)
	} else {
		println("*! Warning: SBOM Author not correctly formed, expecting 'Name <email>' format. Skipping.")
	}  
	sbom.setMetadata(sbomMetadata)
    sbom.setDependencies(new ArrayList<Dependency>())
}

def addEntryToSBOM(DeployableArtifact deployableArtifact, HashMap<String, String> info) {

	String container = info.get("container")
	String owningApplication = info.get("owningApplication")
	Record record = info.get("record")
	PropertiesRecord propertiesRecord = info.get("propertiesRecord")
	DependencySetRecord dependencySetRecord = info.get("dependencySetRecord")
			
	def applicationComponent = sbom.getComponents().find {
		it.getName().equals(owningApplication)
	}
	if (!applicationComponent) {
		applicationComponent = new Component()
		applicationComponent.setType(Component.Type.APPLICATION)
		applicationComponent.setName(owningApplication)
		applicationComponent.setBomRef(newBomRef())
		applicationComponent.setComponents(new ArrayList<Component>())
        sbom.addComponent(applicationComponent)			
	}
    def applicationDependency = sbom.getDependencies().find {
        it.getRef().equals(applicationComponent.getBomRef())
    }
    if (!applicationDependency) {
        applicationDependency = new Dependency(applicationComponent.getBomRef())
        applicationDependency.setDependencies(new ArrayList<Dependency>())
        sbom.getDependencies().add(applicationDependency)
    }
    Component deployableArtifactComponent = new Component()
    deployableArtifactComponent.setType(Component.Type.APPLICATION)
    deployableArtifactComponent.setName(deployableArtifact.file)
    // Set the properties of the Deployable Artifact
    ArrayList<Property> deployableArtifactComponentProperties = new ArrayList<Property>()
    Property deployableArtifactContainerComponentProperty = new Property()
    deployableArtifactContainerComponentProperty.setName("container")
    deployableArtifactContainerComponentProperty.setValue(container)
    deployableArtifactComponentProperties.add(deployableArtifactContainerComponentProperty)
    Property deployableArtifactDeployTypeComponentProperty = new Property()
    deployableArtifactDeployTypeComponentProperty.setName("deployType")
    deployableArtifactDeployTypeComponentProperty.setValue(deployableArtifact.deployType)
    deployableArtifactComponentProperties.add(deployableArtifactDeployTypeComponentProperty)
    deployableArtifactComponent.setProperties(deployableArtifactComponentProperties)
    deployableArtifactComponent.setBomRef(newBomRef())
    // Creates the dependencies for the Deployable Artifact
    Dependency deployableArtifactDependency = new Dependency(deployableArtifactComponent.getBomRef())
    ArrayList<Dependency> deployableArtifactDependencies = new ArrayList<Dependency>()
    // Creates the list of sub-components
    ArrayList<Component> deployableArtifactDependenciesComponents = new ArrayList<Component>()
    // Add the source file itself
    Component sourceFileComponent = new Component()
    sourceFileComponent.setType(Component.Type.APPLICATION)
	String sourceFile = record.getFile()
    sourceFileComponent.setName(sourceFile)
    sourceFileComponent.setBomRef(newBomRef())
    ArrayList<Property> sourceFileProperties = new ArrayList<Property>()
    Property sourceFileGitHashProperty = new Property()
    sourceFileGitHashProperty.setName("gitHash")
    sourceFileGitHashProperty.setValue(getGitHash(sourceFile, propertiesRecord))
    sourceFileProperties.add(sourceFileGitHashProperty)
    sourceFileComponent.setProperties(sourceFileProperties)
    deployableArtifactDependenciesComponents.add(sourceFileComponent)
    deployableArtifactDependencies.add(new Dependency(sourceFileComponent.getBomRef()))
    // Now search for dependencies
    if (dependencySetRecord) {
        dependencySetRecord.getAllDependencies().each { physicalDependency ->
            Component dependencyComponent = new Component()
            dependencyComponent.setType(Component.Type.APPLICATION)
            dependencyComponent.setName(physicalDependency.getLname())
            ArrayList<Property> dependencyProperties = new ArrayList<Property>()
            if (physicalDependency.getFile()) {
                Property fileProperty = new Property()
                fileProperty.setName("file")
                fileProperty.setValue((physicalDependency.getSourceDir() != null ? physicalDependency.getSourceDir() : "") + physicalDependency.getFile())
                dependencyProperties.add(fileProperty)
                def dependencyGitHash = getGitHash(physicalDependency.getFile(), propertiesRecord)
                if (dependencyGitHash != null && dependencyGitHash != "") {
                    Property dependencyGitHashProperty = new Property()
                    dependencyGitHashProperty.setName("gitHash")
                    dependencyGitHashProperty.setValue(dependencyGitHash)
                    dependencyProperties.add(dependencyGitHashProperty)
                }
            }
            if (physicalDependency.getLibrary()) {
                Property libraryProperty = new Property()
                libraryProperty.setName("library")
                libraryProperty.setValue(physicalDependency.getLibrary())
                dependencyProperties.add(libraryProperty)
            }
            if (physicalDependency.getCategory()) {
                Property categoryProperty = new Property()
                categoryProperty.setName("category")
                categoryProperty.setValue(physicalDependency.getCategory())
                dependencyProperties.add(categoryProperty)
            }
            
			dependencyComponent.setBomRef(newBomRef())
			dependencyComponent.setProperties(dependencyProperties)
            deployableArtifactDependencies.add(new Dependency(dependencyComponent.getBomRef()))
            deployableArtifactDependenciesComponents.add(dependencyComponent)
        }
    }
    deployableArtifactComponent.setComponents(deployableArtifactDependenciesComponents)
        
    if (deployableArtifactDependencies && deployableArtifactDependencies.size() > 0) {
        deployableArtifactDependency.setDependencies(deployableArtifactDependencies)
    }
	applicationDependency.getDependencies().add(new Dependency(deployableArtifactComponent.getBomRef()))
    sbom.getDependencies().add(deployableArtifactDependency)
    applicationComponent.getComponents().add(deployableArtifactComponent)
}
	     
    
def writeSBOM(String sbomFilePath, String fileEncoding) {
/*	try {
		FileWriter sbomJSONFileWriter = new FileWriter(sbomFilePath)
		sbomJSONFileWriter.write(BomGeneratorFactory.createJson(CycloneDxSchema.Version.VERSION_15, sbom).toJsonString());
		sbomJSONFileWriter.close()
	} catch (IOException e) {
	} */
	
	File sbomFile = new File(sbomFilePath)
	sbomFile.withWriter(fileEncoding) { writer ->
		writer.write(BomGeneratorFactory.createJson(CycloneDxSchema.Version.VERSION_15, sbom).toJsonString())
	}
	
	
}

def getGitHash(String file, propertiesRecord) {
	String[] fileParts = file.split('/')
	Boolean hashFound = false
	int index = 0
	while (index < fileParts.length && !hashFound) {
		String searchFile = fileParts[0]
		for (int i = 1; i <= index; i++) {
			searchFile = searchFile + '/' + fileParts[i]
		}
		if (propertiesRecord.getProperty(hashPrefix + searchFile) != null) {
		   	return propertiesRecord.getProperty(hashPrefix + searchFile)	
		}
		index++
	}
   	return null
}

def newBomRef() {
	bomRef++
	return String.valueOf(bomRef)
}