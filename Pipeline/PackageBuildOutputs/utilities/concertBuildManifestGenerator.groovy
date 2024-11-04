import groovy.transform.*
import groovy.yaml.YamlBuilder
import com.ibm.dbb.build.report.records.*

/*
 * This is a utility method to generate IBM Concert build files
 */

@Field ConcertBuildManifest concertManifest = new ConcertBuildManifest()

def initConcertBuildManifestGenerator() {
	// Metadata
	concertManifest.concert = new Concertdata()
	concertManifest.concert.builds = new ArrayList<Build>()
}

def addBuild(String application, String version, String buildNumber) {
	Build build = new Build()
	build.repositories = new ArrayList<Repository>()
	build.library = new Library()
	build.component_name = application
	build.number = buildNumber
	build.output_file = application + "_sbom.json"
	// Metadata information
	build.version = version
	concertManifest.concert.builds.add(build)
	return build
}

def addRepositoryToBuild(Build build, String url, String branch, String shortCommit) {
	Repository repository = new Repository()
	repository.name = build.component_name
	repository.url = url
	repository.branch = branch
	repository.commit_sha = shortCommit
	build.repositories.add(repository)
}

def addLibraryInfoTobuild(Build build, String filename, String url) {
	build.library.scope = 'tar'
	build.library.name = build.component_name
	build.library.filename = filename
	build.library.version = build.version
	build.library.url = url
}

def addSBOMInfoToBuild(Build build, String sbomFileName, String sbomSerialNumber) {
	build.library.cyclonedx_bom_link  = new SBOMInfo()
	build.library.cyclonedx_bom_link.file = sbomFileName
	build.library.cyclonedx_bom_link.data = new SBOMdata()
	build.library.cyclonedx_bom_link.data.serial_number = sbomSerialNumber
	build.library.cyclonedx_bom_link.data.version = 1
}

/**
 * Write an Concert Build Manifest  a YAML file
 */
def writeBuildManifest(File yamlFile, String fileEncoding, String verbose){
	println("** Generate IBM Concert Build Manifest file to '$yamlFile'")
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
 * IBM Concert Manifest Classes and Helpers
 */

class ConcertBuildManifest {
    String specVersion = "1.0.3"
    Concertdata concert
}

class Concertdata {
    ArrayList<Build> builds
}

class Build {
    String component_name
    String output_file
    String number 
    String version
    Library library
    ArrayList<Repository> repositories
}

class Repository {
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