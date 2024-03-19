import groovy.transform.*
import groovy.yaml.YamlBuilder
import com.ibm.dbb.build.report.records.*

/*
 * This is a utility method to generate the Wazi Deploy Application Manifest file  
 */

/**
 * Initialize application manifest file
 * Should be the constructor
 */

@Field WaziDeployManifest wdManifest = new WaziDeployManifest()

def initWaziDeployManifestGenerator(Properties props) {
	ArrayList<Artifact> artifacts = new ArrayList()
	wdManifest.artifacts = artifacts

	// Metadata
	Metadata metadata = new Metadata()

	wdManifest.metadata = metadata

	// Annotations
	Annotations annotations = new Annotations()
	annotations.creationTimestamp = props.startTime
	metadata.annotations = annotations
	if (props.application) {
		metadata.description = props.application
		metadata.name = props.application
	} else {
		metadata.description = "UNDEFINED"
		metadata.name = "UNDEFINED"
	}

	// Fields to write Application Manifest file

	PackageInfo packageInfo = new PackageInfo()

	metadata.version = (props.versionName) ? props.versionName : props.startTime
	if (props.application) metadata.name = props.application
}

/**
 *
 */
def appendArtifactToAppManifest(DeployableArtifact deployableArtifact, Record record, PropertiesRecord propertiesRecord){
	Artifact artifact = new Artifact()
	artifact.name = deployableArtifact.file
	def gitHashInfo = retrieveBuildResultProperty (propertiesRecord, "githash")
	artifact.hash =  (gitHashInfo) ? gitHashInfo : "UNDEFINED"
	artifact.description = (record.file) ? record.file : deployableArtifact.file
	if (propertiesRecord) {
		ArrayList<ElementProperty> artifactProperties = new ArrayList()
		["githash", "giturl"].each {property ->
			ElementProperty artifactProperty = new ElementProperty()
			def propValue =  retrieveBuildResultProperty (propertiesRecord, property)
			if (propValue) {
				artifactProperty.key = property
				artifactProperty.value = propValue
				artifactProperties.add(artifactProperty)
			}
		}
		if (artifactProperties.size() > 0 ) {
			artifact.properties = artifactProperties
		}
	}
	artifact.type =deployableArtifact.deployType

	// adding artifact into applicationManifest
	wdManifest.artifacts.add(artifact)
}

def setScmInfo(HashMap<String, String> scmInfoMap) {
	def ScmInfo scmInfo = new ScmInfo()

	scmInfoMap.each { k, v ->
		scmInfo."$k" = v
	}

	wdManifest.metadata.annotations.scmInfo = scmInfo
}

/**
 * Write an Wazi Deploy Manifest  a YAML file
 */
def writeApplicationManifest(File yamlFile, String verbose){
	println("** Generate Wazi Deploy Application Manifest file to $yamlFile")
	def yamlBuilder = new YamlBuilder()

	yamlBuilder {
		apiVersion wdManifest.apiVersion
		kind wdManifest.kind
		metadata wdManifest.metadata
		artifacts  wdManifest.artifacts
	}

	if (verbose && verbose.toBoolean()) {
		println yamlBuilder.toString()
	}

	// write file
	yamlFile.withWriter() { writer ->
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
