import groovy.transform.*
import groovy.yaml.YamlBuilder
import com.ibm.dbb.build.report.records.*

/*
 * This is a utility method to generate the Wazi Deploy Application Manifest file  
 * See https://www.ibm.com/docs/en/developer-for-zos/16.0?topic=files-application-manifest-file
 */

/**
 * Initialize application manifest file
 * Should be the constructor
 */

@Field WaziDeployManifest wdManifest = new WaziDeployManifest()

def initWaziDeployManifestGenerator(Properties props) {
	// Metadata
	wdManifest.metadata = new Metadata()

	if (props.application) {
		wdManifest.metadata.description = props.application
		wdManifest.metadata.name = props.application
	} else {
		wdManifest.metadata.description = "UNDEFINED"
		wdManifest.metadata.name = "UNDEFINED"
	}

	// Metadata information
	wdManifest.metadata.version = (props.versionName) ? props.versionName : props.startTime
	if (props.application) wdManifest.metadata.name = props.application

	// Annotations
	wdManifest.metadata.annotations = new Annotations()
	wdManifest.metadata.annotations.creationTimestamp = props.startTime
}

/**
 * Append Artifact Record to Wazi Deploy Application Manifest 
 */
def appendArtifactToAppManifest(DeployableArtifact deployableArtifact, String path, Record record, PropertiesRecord propertiesRecord){

	Artifact artifact = new Artifact()
	artifact.name = deployableArtifact.file
	artifact.description = (record.file) ? record.file : deployableArtifact.file
	// Add properties
	artifact.properties = new ArrayList()
	ElementProperty pathProperty = new ElementProperty()
	pathProperty.key = "path"
	pathProperty.value = path
	artifact.properties.add(pathProperty)
	// Add optional properties
	if (propertiesRecord) {
		["githash", "giturl"].each {property ->
			ElementProperty artifactProperty = new ElementProperty()
			def propValue =  retrieveBuildResultProperty (propertiesRecord, property)
			if (propValue) {
				artifactProperty.key = property
				artifactProperty.value = propValue
				artifact.properties.add(artifactProperty)
			}
		}
	}
	// add type
	artifact.type =deployableArtifact.deployType

	// add hash
	def gitHashInfo = retrieveBuildResultProperty (propertiesRecord, "githash")
	artifact.hash =  (gitHashInfo) ? gitHashInfo : "UNDEFINED"

	// adding artifact into applicationManifest
	if (!wdManifest.artifacts) wdManifest.artifacts = new ArrayList<Artifact>()
	wdManifest.artifacts.add(artifact)
}

/**
 * Append Artifact Deletion Record to Wazi Deploy Application Manifest 
 */
def appendArtifactDeletionToAppManifest(DeployableArtifact deployableArtifact, String path, Record record, PropertiesRecord propertiesRecord){

	Artifact deleted_artifact = new Artifact()
	deleted_artifact.name = deployableArtifact.file
	deleted_artifact.description = (record.getAttribute("file")) ? record.getAttribute("file") : deployableArtifact.file

	// Add properties
	deleted_artifact.properties = new ArrayList()
	ElementProperty pathProperty = new ElementProperty()
	pathProperty.key = "path"
	pathProperty.value = path
	deleted_artifact.properties.add(pathProperty)

	// Add optional properties
	if (propertiesRecord) {
		["githash", "giturl"].each {property ->
			ElementProperty artifactProperty = new ElementProperty()
			def propValue =  retrieveBuildResultProperty (propertiesRecord, property)
			if (propValue) {
				artifactProperty.key = property
				artifactProperty.value = propValue
				deleted_artifact.properties.add(artifactProperty)
			}
		}
	}
	
	// add type
	deleted_artifact.type = deployableArtifact.deployType

	// add hash
	def gitHashInfo = retrieveBuildResultProperty (propertiesRecord, "githash")
	deleted_artifact.hash =  (gitHashInfo) ? gitHashInfo : "UNDEFINED"

	// adding artifact into applicationManifest
	if (!wdManifest.deleted_artifacts) wdManifest.deleted_artifacts = new ArrayList<Artifact>()
	wdManifest.deleted_artifacts.add(deleted_artifact)

}


def setScmInfo(HashMap<String, String> scmInfoMap) {
	wdManifest.metadata.annotations.scmInfo = new ScmInfo()
	scmInfoMap.each { k, v ->
		wdManifest.metadata.annotations.scmInfo."$k" = v
	}
}

/**
 * Write an Wazi Deploy Manifest  a YAML file
 */
def writeApplicationManifest(File yamlFile, String fileEncoding, String verbose){
	println("** Generate Wazi Deploy Application Manifest file to $yamlFile")
	def yamlBuilder = new YamlBuilder()

	if (wdManifest.artifacts && wdManifest.deleted_artifacts) { 
		yamlBuilder {
			apiVersion wdManifest.apiVersion
			kind wdManifest.kind
			metadata wdManifest.metadata
			artifacts  wdManifest.artifacts
			deleted_artifacts wdManifest.deleted_artifacts
		}
	} else if (wdManifest.artifacts) {
		yamlBuilder {
			apiVersion wdManifest.apiVersion
			kind wdManifest.kind
			metadata wdManifest.metadata
			artifacts  wdManifest.artifacts
		}
	} else if (wdManifest.deleted_artifacts) {
		yamlBuilder {
			apiVersion wdManifest.apiVersion
			kind wdManifest.kind
			metadata wdManifest.metadata
			deleted_artifacts wdManifest.deleted_artifacts
		}
	} else {
		println "*!* Generate WaziDeployManifest did not recorded any artifacts or deleted_artifacts."
	}
		

	if (verbose && verbose.toBoolean()) {
		println yamlBuilder.toString()
	}

	// write file
	yamlFile.withWriter(fileEncoding) { writer ->
		writer.write(yamlBuilder.toString())
	}
}

private def retrieveBuildResultProperty(PropertiesRecord buildResultPropertiesRecord, String propertyName) {

	if (buildResultPropertiesRecord!=null) {
		def buildResultProperties = buildResultPropertiesRecord.getProperties()

		def property = buildResultProperties.find {
			it.key.contains(propertyName)
		}

		if (property) {
			return property.getValue()
		} else
		{
			return null
		}
	}
}

/**
 * Wazi Deploy Application Manifest Classes and Helpers
 */

class WaziDeployManifest {
	String apiVersion = "wazideploy.ibm.com/v1"
	String kind = "ManifestState"
	Metadata metadata
	ArrayList<Artifact> artifacts
	ArrayList<Artifact> deleted_artifacts
}

class Metadata {
	String name
	String description
	String version
	Annotations annotations
}

class Annotations {
	String creationTimestamp
	ScmInfo scmInfo
	PackageInfo packageInfo
}

class ScmInfo {
	String type
	String uri
	String branch
	String shortCommit
}

class PackageInfo {
	String name
	String description
	Properties properties
	String uri
	String type
}

class Artifact {
	String name
	String description
	ArrayList<ElementProperty> properties
	String type
	String hash
}

class ElementProperty {
	String key
	String value
}