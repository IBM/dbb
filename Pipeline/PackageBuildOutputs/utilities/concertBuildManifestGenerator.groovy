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

@Field ConcertBuildManifest concertManifest = new ConcertBuildManifest()

def initConcertBuildManifestGenerator(Properties props, String buildNumber) {
	// Metadata
	concertManifest.concert = new Concertdata()
	concertManifest.concert.builds = new Builds()
	concertManifest.concert.builds[0].repositories = new Repositories()
	concertManifest.concert.builds[0].library = new Library()

	if (props.application) {
		concertManifest.concert.builds[0].component_name = props.application
	} else {
		concertManifest.concert.builds[0].component_name = "UNDEFINED"
	} 
	concertManifest.concert.builds[0].number = buildNumber
	concertManifest.concert.builds[0].output_file = concertManifest.concert.builds[0].component_name + "_sbom.json"

	// Metadata information
	concertManifest.concert.builds[0].version = (props.versionName) ? props.versionName : props.startTime

}

def setScmInfo(HashMap<String, String> scmInfoMap) {
	concertManifest.concert.builds[0].repositories[0].name = concertManifest.concert.builds[0].component_name
	concertManifest.concert.builds[0].repositories[0].url = scmInfoMap.uri
	concertManifest.concert.builds[0].repositories[0].branch = scmInfoMap.branch
	concertManifest.concert.builds[0].repositories[0].commit_sha = scmInfoMap.shortCommit
}

def setPackageInfo(HashMap<String, String> packageInfoMap) {
	concertManifest.concert.builds[0].library.scope = 'tar'
	concertManifest.concert.builds[0].library.name = concertManifest.concert.builds[0].component_name
	concertManifest.concert.builds[0].library.filename = packageInfoMap.name
	concertManifest.concert.builds[0].library.version = concertManifest.concert.builds[0].version
	concertManifest.concert.builds[0].library.url = packageInfoMap.uri
}

def setSBOMInfo(String sbomFileName, String sbomSerialNumber) {
	concertManifest.concert.builds[0].library.cyclonedx_bom_link  = new SBOMInfo()
	concertManifest.concert.builds[0].library.cyclonedx_bom_link.file = sbomFileName
	concertManifest.concert.builds[0].library.cyclonedx_bom_link.data = new SBOMdata()
	concertManifest.concert.builds[0].library.cyclonedx_bom_link.data.serial_number = sbomSerialNumber
	concertManifest.concert.builds[0].library.cyclonedx_bom_link.data.version = 1
}

/**
 * Write an Concert Build Manifest  a YAML file
 */
def writeBuildManifest(File yamlFile, String fileEncoding, String verbose){
	println("** Generate Concert Build  Manifest file to $yamlFile")
	def yamlBuilder = new YamlBuilder()
    
	yamlBuilder {
			specVersion concertManifest.specVersion
			concert concertManifest.concert
		}

	if (verbose && verbose.toBoolean()) {
		println yamlBuilder.toString()
	}

	// write file
	yamlFile.withWriter(fileEncoding) { writer ->
		writer.write(yamlBuilder.toString())
	}
}

/**
 * Concert Deploy  Manifest Classes and Helpers
 */

class ConcertBuildManifest {
    String specVersion = "1.0.2"
    Concertdata concert
}

class Concertdata {
    Builds[] builds
}

class Builds {
    String component_name
    String output_file
    String number 
    String version
    Library library
    Repositories[] repositories
}

class Repositories {
    String name
    String url
    String branch
    String commit_sha
}

class Library {
    String scope
	String name
    String version
	String filename
    String url
    SBOMInfo cyclonedx_bom_link
}

class SBOMInfo {
    String file
	SBOMdata data
}

class SBOMdata {
    String serial_number
	String version
}