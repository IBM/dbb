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
import groovy.io.FileType
import java.nio.file.*
import static java.nio.file.StandardCopyOption.*
import com.ibm.jzos.ZFile;

/************************************************************************************
 * This script creates a simplified package with the outputs generated from a DBB build
 * Optionally, it publishes it to an Artifact repository.
 *
 * usage: see help command or README
 *
 * Version 0 - 2019
 *  called PublishLoadModule.groovy and located in Build/PublishLoadModules
 *
 * Version 1 - 2021
 *  Re-Design to run as a post-build script and make publishing optional
 *
 * Version 2 - 2022-02
 *  - Externalize the Map of LLQ to CopyMode
 *  - Add capablity to add additional files from build workspace
 *  - Verbose logging will print tar contents
 *
 * Version 3 - 2022-08
 *  - Ability to pass multiple build reports to build a cumulative package
 *  - Add an optional option to add the deploy type extension to the member
 *
 * Version 4 - 2022-12
 *  - Generalized the options to publish package to artifact repository
 *  - Script is supporting both JFrog Artifactory and SonarType Nexus
 *  - Added additional CLI option to pass artifact repository parameters
 *
 * Version 5 - 2023-07
 *  - Added support for DeployableArtifact, to manage the correct stacking
 *    of duplicates artifacts
 * 
 * Version 6 - 2024-03
 *  - Added support to write IBM Wazi Deploy Application Manifest file
 *      
 * Version 7 - 2024-04
 *  - Added support to SBOM files
 *      
 * Version 8 - 2024-07
 *  - Reworked error management and fixed few glitches
 *      
 * Version 9 - 2024-10
 *  - Added the following
 *    a) Fields and code to generate concert manifest linking to 
 *       concertBuildManifestGenerator.groovy
 *    b) Refactoring to generate SBOM details like SerialNumber from this script  
 *       to be passed to sbomGenerator.groovy and concertBuildManifestGenerator.groovy
 *
 * Version 10 - 2024-11
 *  - Enhanced the packaging with the Application Descriptor file
 *    and baseline packages which allow full description of interfaces
 *
 ************************************************************************************/

// start create & publish package
@Field Properties props = new Properties()
def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
@Field def wdManifestGeneratorUtilities = loadScript(new File("${scriptDir}/utilities/WaziDeployManifestGenerator.groovy"))
@Field def artifactRepositoryHelpers = loadScript(new File("${scriptDir}/ArtifactRepositoryHelpers.groovy"))

@Field def applicationDescriptorUtils
@Field def applicationDescriptor
@Field def sbomUtilities
@Field def concertManifestGeneratorUtilities
@Field def rc = 0
@Field def sbomSerialNumber
@Field def sbomFileName
@Field def concertBuild
@Field String includeSubfolder = "include"
@Field String libSubfolder = "lib"
@Field String binSubfolder = "bin"
@Field def tempLoadDir
@Field def String tarFileLabel = "Default"
@Field def String tarFileName = ""


parseInput(args)


startTime = new Date()
props.startTime = startTime.format("yyyyMMdd.HHmmss.SSS")
println("** PackageBuildOutputs start at $props.startTime")
println("** Properties at startup:")
props.sort().each { k,v->
	if ( k == "artifactRepository.password" )
		println "   $k -> xxxxxx "
	else
		println "   $k -> $v"
}
if (rc != 0) {
	println("*! [ERROR] One or several properties were missing in the configuration. Review the console output.")
	System.exit(rc)
}


// Enable file tagging
BuildProperties.setProperty("dbb.file.tagging", "true") // Enable dbb file tagging

// Map of last level dataset qualifier to DBB CopyToHFS CopyMode.
def copyModeMap = parseCopyModeMap(props.copyModeMap)


// Hashmap of BuildOutput to Record
Map<DeployableArtifact, Map> buildOutputsMap = new HashMap<DeployableArtifact, Map>()

// Field to store default tarFileLabel (buildInfo.label) when cli argument tarFileName is not passed.

// Field to store build number, set to default when its not decipherable from build report
def String buildNumber = "UNKNOWN"

// Object to store scm information for Wazi Deploy Application Manifest file
HashMap<String,String> scmInfo = new HashMap<String, String>()

// Package the public Include Files and service submodules
// if Path to Application Descriptor file is specified
if (props.publishInterfaces && props.publishInterfaces.toBoolean()) {
	File applicationDescriptorFile = new File("${props.applicationFolderPath}/applicationDescriptor.yml")
	if (applicationDescriptorFile.exists()) {
		applicationDescriptorUtils = loadScript(new File("utilities/applicationDescriptorUtils.groovy"))
		applicationDescriptor = applicationDescriptorUtils.readApplicationDescriptor(applicationDescriptorFile)
	} else {
		println("*! [ERROR] No Application Descriptor file '${props.applicationFolderPath}/applicationDescriptor.yml' found. Exiting.")
		System.exit(1)
	}
}

// iterate over all build reports to obtain build output
props.buildReportOrder.each { buildReportFile ->
	Map<DeployableArtifact, Map> temporaryBuildOutputsMap = new HashMap<DeployableArtifact, Map>()
	println("** Read build report data from '${buildReportFile}'.")
	def jsonOutputFile = new File(buildReportFile)

	if (!jsonOutputFile.exists()){
		println("*! [ERROR] Build Report '$buildReportFile' not found.")
		rc = 1
	} else {
		def buildReport= BuildReport.parse(new FileInputStream(jsonOutputFile))

		// Read buildInfo to obtain build information
		def buildInfo = buildReport.getRecords().findAll{
			try {
				it.getType()==DefaultRecordFactory.TYPE_BUILD_RESULT
			} catch (Exception e){}
		}
		if (buildInfo.size() != 0) {
			tarFileLabel = buildInfo[0].label
			buildNumber = buildInfo[0].label
		}

		// retrieve the buildResultPropertiesRecord
		def buildResultPropertiesRecord = buildReport.getRecords().find {
			try {
				it.getType()==DefaultRecordFactory.TYPE_PROPERTIES && it.getId()=="DBB.BuildResultProperties"
			} catch (Exception e){}
		}

		// finds all the build outputs with a deployType
		def buildRecords = buildReport.getRecords().findAll{
			try {
				(it.getType()==DefaultRecordFactory.TYPE_EXECUTE || it.getType()==DefaultRecordFactory.TYPE_COPY_TO_PDS) &&
						!it.getOutputs().isEmpty()
			} catch (Exception e){}
		}

		// finds all the build outputs with a deployType
		// Today the USS_RECORD type is built using an AnyTypeRecord record
		// An Idea is currently opened to have an official USS_RECORD: https://ideas.ibm.com/ideas/DBB-I-43
		def ussBuildRecords = buildReport.getRecords().findAll{
			try {
				it.getType()=="UNIX" && !it.getOutputs().isEmpty()
			} catch (Exception e){}
		}

		// find all deletions using the DELETE_RECORD of zAppBuild
		def deletionRecords = buildReport.getRecords().findAll {
			try {
				// Obtain delete records, which got added by zAppBuild
				it.getType() == "DELETE_RECORD"
			} catch (Exception e) {
				println e
			}
		}

		if (props.deployTypeFilter) {
			println("** Filter Output Records on following deployTypes: ${props.deployTypeFilter}...")
			buildRecords.each {
				// filtered executes
				def filteredOutputs =  it.getOutputs().findAll { o ->
					o.deployType != null && (props.deployTypeFilter).split(',').contains(o.deployType)
				}
				// Manipulating the scope of build outputs
				it.getOutputs().clear()
				it.getOutputs().addAll(filteredOutputs)
			}
			ussBuildRecords.each {
				ArrayList<ArrayList> outputs = []
				it.getAttribute("outputs").split(';').collectEntries { entry ->
					outputs += entry.replaceAll('\\[|\\]', '').split(',')
				}

				ArrayList<String> filteredOutputs = []
				outputs.each { output ->
					rootDir = output[0].trim()
					file = output[1].trim()
					deployType = output[2].trim()
					if (!(props.deployTypeFilter).split(',').contains(deployType)) {
						filteredOutputs += output.toString()
					}
				}

				def filteredOutputsStrings = String.join(";", filteredOutputs)
				it.setAttribute("outputs", filteredOutputsStrings)
			}
		} else {
			// Remove outputs without deployType + ZUNIT-TESTCASEs
			println("** Remove output records without deployType or with deployType=ZUNIT-TESTCASE")
			buildRecords.each {
				def unwantedOutputs =  it.getOutputs().findAll{ o ->
					o.deployType == null || o.deployType == 'ZUNIT-TESTCASE'
				}
				it.getOutputs().removeAll(unwantedOutputs)
			}
		}

		buildRecords += ussBuildRecords // append USS records

		def datasetMembersCount = 0
		def zFSFilesCount = 0
		def deletionCount = 0

		// adding files and executes with outputs to Hashmap to remove redundant data
		buildRecords.each{ buildRecord ->
			if (buildRecord.getType()=="UNIX") {
				if (!buildRecord.getOutputs().isEmpty()) {
					ArrayList<ArrayList> outputs = buildRecord.getOutputs()
					zFSFilesCount += outputs.size()
					outputs.each{ output ->
						rootDir = output.rootDir
						file = output.file
						deployType = output.deployType
						def dependencySetRecord = buildReport.getRecords().find {
							it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET && it.getFile().equals(file)
						}

						temporaryBuildOutputsMap.put(new DeployableArtifact(file, deployType, "zFSFile"), [
							container: rootDir,
							owningApplication: props.application,
							record: buildRecord,
							propertiesRecord: buildResultPropertiesRecord,
							dependencySetRecord: dependencySetRecord
						])
					}
				}
			} else {
				if (buildRecord.getOutputs().size() != 0) {
					buildRecord.getOutputs().each { output ->
						def (dataset, member) = getDatasetName(output.dataset)
						def fileUsage
						if (applicationDescriptor && output.deployType.equals("OBJ")) {
							fileUsage = applicationDescriptorUtils.getFileUsageByType(applicationDescriptor, "Program", member)
						}
						// If the artifact is not an Object Deck or has no usage or its usage is not main
						if ((output.deployType.equals("OBJ") && fileUsage && (fileUsage.equals("internal submodule") || fileUsage.equals("service submodule"))) || !output.deployType.equals("OBJ")) {
							datasetMembersCount++
							String file = buildRecord.getFile()
							def dependencySetRecord = buildReport.getRecords().find {
								it.getType()==DefaultRecordFactory.TYPE_DEPENDENCY_SET && it.getFile().equals(file)
							}
							temporaryBuildOutputsMap.put(new DeployableArtifact(member, output.deployType, "DatasetMember"), [
								container: dataset,
								owningApplication: props.application,
								record: buildRecord,
								propertiesRecord: buildResultPropertiesRecord,
								dependencySetRecord: dependencySetRecord
							])
						} else {
							if (props.verbose) println("*! Build output ${output.dataset} with deployType '${output.deployType}' has been excluded from packaging.")
						}
					}
				}
			}
		}

		deletionRecords.each { deleteRecord ->
			deletionCount += deleteRecord.getAttributeAsList("deletedBuildOutputs").size()
			deleteRecord.getAttributeAsList("deletedBuildOutputs").each{ deletedFile ->

				String cleansedDeletedFile = ((String) deletedFile).replace('"', '');
				def (dataset, member) = getDatasetName(cleansedDeletedFile)

				// search for an existing deployableArtifacts record
				ArrayList<DeployableArtifact> filteredDeployableArtifacts = new ArrayList()

				temporaryBuildOutputsMap.each { DeployableArtifact deployableArtifact, Map info ->
					if (deployableArtifact.file == deleteRecord.getAttribute("file")) {
						filteredDeployableArtifacts.add(deployableArtifact, info)
					}
				}

				if (filteredDeployableArtifacts){
					filteredDeployableArtifacts.each {deployableArtifact, info ->
						String container = info.get("container")
						if (container == dataset && member == deployableArtifact.file) {
							deployType = deployableArtifact.deployType
							// remove any existing change
							temporaryBuildOutputsMap.remove(deployableArtifact)
							// add deletion
							temporaryBuildOutputsMap.put(new DeployableArtifact(member, deployType, "DatasetMemberDelete"), [
								container: dataset,
								owningApplication: props.application,
								record: buildRecord,
								propertiesRecord: buildResultPropertiesRecord,
								dependencySetRecord: dependencySetRecord
							])
						}
					}
				} else {
					deployType = dataset.replaceAll(/.*\.([^.]*)/, "\$1") // DELETE_RECORD does not contain deployType attribute. Use LLQ
					temporaryBuildOutputsMap.put(new DeployableArtifact(member, deployType, "DatasetMemberDelete"), [
						container: dataset,
						owningApplication: props.application,
						record: deleteRecord,
						propertiesRecord: buildResultPropertiesRecord
					])
				}
			}
		}


		// Print summary of BuildReport
		if ( datasetMembersCount + zFSFilesCount == 0 ) {
			println("** No items to package in '$buildReportFile'.")
		} else {
			println("** ${temporaryBuildOutputsMap.size()} Build outputs detected in '$buildReportFile':")
			temporaryBuildOutputsMap.each { deployableArtifact, info ->
				String container = info.get("container")
				String owningApplication = info.get("owningApplication")
				Record record = info.get("record")
				PropertiesRecord propertiesRecord = info.get("propertiesRecord")
				DependencySetRecord dependencySetRecord = info.get("dependencySetRecord")
				println("\t'${deployableArtifact.file}' from '${container}' with Deploy Type '${deployableArtifact.deployType}'")
			}
		}

		// Log detected deleted files
		if (deletionCount != 0) {
			println("**  Deleted files detected in '$buildReportFile':")
			deletionRecords.each { it.getAttributeAsList("deletedBuildOutputs").each { println("   ${it}")}}
		}

		buildOutputsMap.putAll(temporaryBuildOutputsMap)

		// generate scmInfo for Wazi Deploy Application Manifest file
		if ((props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) ||
				(props.generateConcertBuildManifest  && props.generateConcertBuildManifest.toBoolean() )) {

			if (props.buildReportOrder.size() == 1) {
				scmInfo.put("type", "git")
				gitUrl = retrieveBuildResultProperty (buildResultPropertiesRecord, "giturl")
				if (gitUrl) scmInfo.put("uri", gitUrl)
				gitHash = retrieveBuildResultProperty (buildResultPropertiesRecord, "githash")
				if (gitHash) scmInfo.put("shortCommit", gitHash)
				scmInfo.put("branch", props.branch)
			} else {
				scmInfo.put("shortCommit", "multipleBuildReports")
				scmInfo.put("uri", "multipleBuildReports")
			}
		}
	}
}

if (rc == 0) {

	if (buildOutputsMap.size() == 0) {
		println("** There are no build outputs found in all provided build reports. Exiting.")
		rc = 0
	} else {
		// Initialize Wazi Deploy Manifest Generator
		if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
			wdManifestGeneratorUtilities.initWaziDeployManifestGenerator(props)// Wazi Deploy Application Manifest
			wdManifestGeneratorUtilities.setScmInfo(scmInfo)
			if (props.externalDependenciesEvidences) {
				File externalDependenciesEvidenceFile = new File("${props.externalDependenciesEvidences}")
				if (externalDependenciesEvidenceFile.exists()){
					wdManifestGeneratorUtilities.setExternalDependencies(externalDependenciesEvidenceFile)
				} else {
					println("** External build dependencies file not found (${props.externalDependenciesEvidences}). Exiting.")
					rc=4
				}
			}
		}

		// Initialize SBOM
		if (props.generateSBOM && props.generateSBOM.toBoolean()) {
			sbomUtilities = loadScript(new File("${scriptDir}/utilities/sbomGenerator.groovy"))
			sbomSerialNumber = "url:uuid:" + UUID.randomUUID().toString()
			sbomFileName = "${buildNumber}_sbom.json"
			sbomUtilities.initializeSBOM(props.sbomAuthor, sbomSerialNumber)
		}

		// Initialize Concert Build Manifest Generator
		if (props.generateConcertBuildManifest && props.generateConcertBuildManifest.toBoolean()) {
			// Concert Build Manifest

			concertManifestGeneratorUtilities = loadScript(new File("${scriptDir}/utilities/concertBuildManifestGenerator.groovy"))
			concertManifestGeneratorUtilities.initConcertBuildManifestGenerator()
			concertBuild = concertManifestGeneratorUtilities.addBuild(props.application, props.versionName, buildNumber)
			concertManifestGeneratorUtilities.addRepositoryToBuild(concertBuild, scmInfo.uri, scmInfo.branch, scmInfo.shortCommit)
		}

		// Local variables
		tarFileName = (props.tarFileName) ? props.tarFileName  : "${tarFileLabel}.tar"
		def tarFile = "$props.workDir/${tarFileName}"

		//Create a temporary directory on zFS to copy the load modules from data sets to
		tempLoadDir = new File("$props.workDir/tempPackageDir")
		!tempLoadDir.exists() ?: tempLoadDir.deleteDir()
		tempLoadDir.mkdirs()

		// A baseline Package has been specified, we then extract it in the $tempLoadDir folder
		if (props.baselinePackageFilePath) {
			File baselinePackageFile = new File(props.baselinePackageFilePath)
			if (baselinePackageFile.exists()) {
				println("** Extract the baseline package from '${props.baselinePackageFilePath}'")
				def processCmd = [
					"sh",
					"-c",
					"tar -xUXf ${props.baselinePackageFilePath}"
				]

				def processRC = runProcess(processCmd, tempLoadDir)
				rc = Math.max(rc, processRC)
				if (rc == 0) {
					println("** Baseline Package '${props.baselinePackageFilePath}' successfully extracted.")

					// Read the existing Wazi Deploy Manifest if any
					File wdManifestFile = new File("$tempLoadDir/wazideploy_manifest.yml")
					if (wdManifestFile.exists()) {
						if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
							// Read the manifest file if it exists
							wdManifestGeneratorUtilities.readWaziDeployManifestFile(wdManifestFile, props)
							wdManifestGeneratorUtilities.setScmInfo(scmInfo)
						} else {
							wdManifestFile.delete()
						}
					} else {
						if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
							// Otherwise initialize an empty manifest
							wdManifestGeneratorUtilities.initWaziDeployManifestGenerator(props)
							wdManifestGeneratorUtilities.setScmInfo(scmInfo)
						}
					}

					// Search in all subfolders of the archive except the folders
					// that contains includes "$includeSubfolder" and binaries "$binSubfolder"
					// Copy the artifacts found to comply with the right structure
					// All the artifact that don't comply will end up in the binSubfolder
					tempLoadDir.eachDir() { subfolder ->
						if (!subfolder.getName().equals(includeSubfolder) && !subfolder.getName().equals(libSubfolder)) {
							subfolder.eachFileRecurse(FileType.FILES) { file ->
								String fileName = file.getName()
								def fileNameParts = fileName.split("\\.")
								if (fileNameParts.size() > 1) {
									fileName = fileNameParts.first()
									String fileDeployType = fileNameParts.last()
									if (props.fullPackage && props.fullPackage.toBoolean()) {
										String expectedFilePath = "$tempLoadDir/$binSubfolder/$fileDeployType/$fileName"
										try {
											Path destinationPath = Paths.get("$tempLoadDir/$binSubfolder/$fileDeployType/${fileName}.${fileDeployType}")
											Path destinationDirPath = destinationPath.getParent()
											destinationDirPath.toFile().mkdirs()
											Path sourcePath = file.toPath()
											copyFiles(sourcePath.toString(), destinationPath.toString())
											println("\tCopy file '${sourcePath}' to '${destinationPath}'")
											if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
												// Update Path for the moved file in Wazi Deploy Manifest
												rc = rc + wdManifestGeneratorUtilities.updateArtifactPathToManifest(fileName, fileDeployType, "$binSubfolder/$fileDeployType/${fileName}.${fileDeployType}")
											}
										} catch (IOException e) {
											println("!* [ERROR] Error when moving file '${sourcePath}' to '${destinationPath}' during baseline package extraction.")
											rc = 1
										}
									} else {
										file.delete()
										if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
											wdManifestGeneratorUtilities.removeArtifactFromManifest(fileName, fileDeployType)
										}
									}
								}
							}
							subfolder.deleteDir()
						}
						if (subfolder.getName().equals("tmp")) {
							subfolder.deleteDir()
						}
					}
				} else {
					println("*! [ERROR] Error when extracting baseline package '${created}' with rc=$rc.")
					rc = 1
				}
			} else {
				println("*! [ERROR] The Baseline Package '${props.baselinePackageFilePath}' was not found.")
				rc = 1
			}
		}

		if (rc == 0) {

			println("** Total number of build outputs to package: ${buildOutputsMap.size()}")

			def publicInterfacesDeployTypes
			def privateInterfacesDeployTypes
			def processedArtifacts = 0 // used as a checksum that all files got categorized

			if (props.publicInterfacesDeployTypes) {
				publicInterfacesDeployTypes = props.publicInterfacesDeployTypes.split(",") // split comma separated list into list
			} else {
				println("*! [WARNING] Property 'publicInterfacesDeployTypes' not defined, using default types 'OBJ'.")
				publicInterfacesDeployTypes = ["OBJ"]
			}
			if (props.privateInterfacesDeployTypes) {
				privateInterfacesDeployTypes = props.privateInterfacesDeployTypes.split(",") // split comma separated list into list
			} else {
				println("*! [WARNING] Property 'privateInterfacesDeployTypes' not defined, using default types 'OBJ,BMSCOPY'.")
				privateInterfacesDeployTypes = "OBJ,BMSCOPY".split(",")
			}

			def deployableOutputs = buildOutputsMap.findAll { deployableArtifact, info ->
				!((publicInterfacesDeployTypes && publicInterfacesDeployTypes.contains(deployableArtifact.deployType)) || (privateInterfacesDeployTypes && privateInterfacesDeployTypes.contains(deployableArtifact.deployType)))
			}
			if (deployableOutputs && !deployableOutputs.isEmpty()) {
				println("** Copy ${deployableOutputs.size()} deployable artifacts to temporary package directory '$tempLoadDir/$binSubfolder'")
				copyArtifactsToUSS(deployableOutputs, binSubfolder, copyModeMap)
				processedArtifacts += deployableOutputs.size()
			}

			if (props.publishInterfaces && props.publishInterfaces.toBoolean()) {

				def publicInterfaces
				if (publicInterfacesDeployTypes) {
					// build outputs that are mapped to a public deployType and are flagged as 'service submodule' in the application descriptor
					publicInterfaces = buildOutputsMap.findAll { deployableArtifact, info ->
						if (deployableArtifact.deployType.equals("OBJ")) {
							fileUsage = applicationDescriptorUtils.getFileUsageByType(applicationDescriptor, "Program", deployableArtifact.file)
							publicInterfacesDeployTypes.contains(deployableArtifact.deployType) && fileUsage && fileUsage.equals("service submodule")
						} else {
							publicInterfacesDeployTypes.contains(deployableArtifact.deployType)
						}
					}
					if (publicInterfaces && !publicInterfaces.isEmpty()) {
						println("** Copy ${publicInterfaces.size()} public interfaces to temporary package directory '$tempLoadDir/$includeSubfolder'")
						copyArtifactsToUSS(publicInterfaces, includeSubfolder, copyModeMap)
						processedArtifacts += publicInterfaces.size()
					}
				}
				def privateInterfaces
				if (privateInterfacesDeployTypes) {
					// build outputs that are mapped to a public deployType and are flagged as 'service submodule' in the application descriptor
					privateInterfaces = buildOutputsMap.findAll { deployableArtifact, info ->
						if (deployableArtifact.deployType.equals("OBJ")) {
							fileUsage = applicationDescriptorUtils.getFileUsageByType(applicationDescriptor, "Program", deployableArtifact.file)
							privateInterfacesDeployTypes.contains(deployableArtifact.deployType) && fileUsage && fileUsage.equals("internal submodule")
						} else {
							privateInterfacesDeployTypes.contains(deployableArtifact.deployType)
						}
					}
					println("** Copy ${privateInterfaces.size()} private interfaces to temporary package directory '$tempLoadDir/$libSubfolder'")
					copyArtifactsToUSS(privateInterfaces, libSubfolder, copyModeMap)
					processedArtifacts += privateInterfaces.size()
				}
				if ((publicInterfaces && !publicInterfaces.isEmpty()) ||
						(privateInterfaces && !privateInterfaces.isEmpty())) {
					// Checks if all binary interfaces (submodules) are in the archive or not
					println("** Validate if all interfaces known in Application Descriptor are packaged.")
					checkBinaryInterfaces(tempLoadDir, applicationDescriptor)
				}

				ArrayList<String> publicIncludeFiles = applicationDescriptorUtils.getFilesByTypeAndUsage(applicationDescriptor, "Include File", "public")
				ArrayList<String> sharedIncludeFiles = applicationDescriptorUtils.getFilesByTypeAndUsage(applicationDescriptor, "Include File", "shared")
				ArrayList<String> allIncludeFiles = new ArrayList<String>()

				if (publicIncludeFiles && !publicIncludeFiles.isEmpty()) {
					allIncludeFiles.addAll(publicIncludeFiles)
				}
				if (sharedIncludeFiles && !sharedIncludeFiles.isEmpty()) {
					allIncludeFiles.addAll(sharedIncludeFiles)
				}

				if (!allIncludeFiles.isEmpty()) {
					println("** Copy ${allIncludeFiles.size()} public/shared Include Files from Application Folder to temporary package directory '$tempLoadDir'")

					allIncludeFiles.forEach() { includeFile ->
						Path includeFilePath = Paths.get("${props.applicationFolderPath}/${includeFile}")
						Path targetIncludeFilePath = Paths.get("${tempLoadDir.getPath()}/${includeSubfolder}/src/${includeFilePath.getFileName()}")
						try {
							//Create target parent folder if it doesn't exist
							def targetIncludeFilesFolder = targetIncludeFilePath.getParent().toFile()
							if (!targetIncludeFilesFolder.exists()) {
								targetIncludeFilesFolder.mkdirs()
							}
							println("\tCopy '${includeFilePath}' file to '${targetIncludeFilePath}'")
							copyFiles(includeFilePath.toString(), targetIncludeFilePath.toString())
						} catch (IOException exception) {
							println "!* [ERROR] Copy failed: an error occurred when copying '${includeFilePath}' to '${targetIncludeFilePath}'"
							rc = Math.max(rc, 1)
						}
					}
				}
			}

			if (processedArtifacts != buildOutputsMap.size()) {
				println("*! [WARNING] The number of copied artifacts ($processedArtifacts) doesn't match the number of identified build outputs (${buildOutputsMap.size()}). Some files might have an incorrect 'usage' in the Application Descriptor.")
			}
		}

		if (props.generateSBOM && props.generateSBOM.toBoolean() && rc == 0) {
			sbomUtilities.writeSBOM("$tempLoadDir/$sbomFileName", props.fileEncoding)
		}

		if (wdManifestGeneratorUtilities && props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean() && rc == 0) {
			if (props.publish && props.publish.toBoolean()) {
				HashMap<String,String> packageInfo = new HashMap<String, String>()
				packageInfo.put("type", "artifactRepository")
				packageInfo.put("name", props.buildIdentifier)
				packageUrl = artifactRepositoryHelpers.computeArchiveUrl(props)
				if (packageUrl) packageInfo.put("uri", packageUrl)
				wdManifestGeneratorUtilities.setPackageInfo(packageInfo)
			}
			// print application manifest
			// wazideploy_manifest.yml is the default name of the manifest file
			wdManifestGeneratorUtilities.writeApplicationManifest(new File("$tempLoadDir/wazideploy_manifest.yml"), props.fileEncoding, props.verbose)
		}

		if (rc == 0) {

			// log buildReportOrder file and add build reports to tar file
			File buildReportOrder = new File("$tempLoadDir/buildReportOrder.txt")
			ArrayList<String> buildReportOrderLines = new ArrayList<String>()

			if (buildReportOrder.exists()) {
				String line
				buildReportOrder.withReader(props.fileEncoding) { reader ->
					while ((line = reader.readLine()) != null && !line.equals("")) {
						buildReportOrderLines.add(line)
					}
				}
			}

			println("** Generate package build report order file to '$buildReportOrder'")

			props.buildReportOrder.each { buildReportFile ->
				Path buildReportFilePath = Paths.get(buildReportFile)

				// Always prefix the buildreport with sequence number
				int nextIndex = buildReportOrderLines.size() + 1
				Path copiedBuildReportFilePath = Paths.get(tempLoadDir.getPath() + "/" + "$nextIndex".padLeft(3, "0") + "_" + buildReportFilePath.getFileName().toString())

				copyFiles(buildReportFilePath.toString(), copiedBuildReportFilePath.toString())
				buildReportOrderLines.add("${copiedBuildReportFilePath.getFileName().toString()}\n")
			}
			buildReportOrder.withWriter(props.fileEncoding) { writer ->
				buildReportOrderLines.each { line ->
					if (!line.isEmpty()) {
						File buildReportFilePath = new File(line)
						String buildReportFileName = buildReportFilePath.getName()
						writer.write("$buildReportFileName\n")
					}
				}
			}

			Path packagingPropertiesFilePath = Paths.get(props.packagingPropertiesFile)
			Path copiedPackagingPropertiesFilePath = Paths.get(tempLoadDir.getPath() + "/" + packagingPropertiesFilePath.getFileName().toString())
			if (props.verbose) println("** Copy packaging properties config file to '$copiedPackagingPropertiesFilePath'")
			copyFiles(packagingPropertiesFilePath.toString(), copiedPackagingPropertiesFilePath.toString())

			if (props.owner) {
				def processCmd = [
					"sh",
					"-c",
					"chown -R ${props.owner} $tempLoadDir/*"
				]
				def processRC = runProcess(processCmd, tempLoadDir)
				rc = Math.max(rc, processRC)
				if (rc == 0) {
					println("** Ownership of files in '$tempLoadDir' successfully changed to '${props.owner}'.")
				} else {
					println("*! [ERROR] Error when changing ownership to '${props.owner}' with rc=$rc.")
				}
			}

			//Package additional outputs to tar file.
		        if (props.includeLogs && rc == 0) {
			       def workDir = new File(props.workDir)
			       (props.includeLogs).split(",").each { fileExtension ->
			       def matchedFiles = []

				println("** Checking for files matching file extension '${fileExtension}' in '${workDir.absolutePath}'.")

				// Convert '*.ext' to extension (like ".log") for basic matching
				if (fileExtension ==~ /\*\.[a-zA-Z0-9]+/) {
					def ext = fileExtension.replace("*", "")
					if (workDir.exists() && workDir.isDirectory()) {
					    workDir.eachFile(FileType.FILES) { file ->
						if (file.name.endsWith(ext)) {
						    matchedFiles << file
						}
					    }
					}
				} else {
					println("*! [WARNING] Unsupported file extension format '${fileExtension}'. Skipping.")
				}

				if (!matchedFiles.isEmpty()) {
                                     def logDir = new File(tempLoadDir, "log")
                                     logDir.mkdirs()  // Create the 'log' directory if it doesn't exist
				     matchedFiles.each() { file ->
                                        def destFile = new File(logDir, file.name)
                                        copyFiles(file.absolutePath, destFile.absolutePath)
					println("       Copy '${file.name}' to '${destFile.absolutePath}'")
                                     }
                                } else {
                                        println("*! [WARNING] No files matching file extension '${fileExtension}' were found in '${props.workDir}'. Skipping.")                                }
                               
                           }
                      }
			if (rc == 0) {
				println("** Create tar file at ${tarFile}")
				// Note: https://www.ibm.com/docs/en/zos/2.4.0?topic=scd-tar-manipulate-tar-archive-files-copy-back-up-file
				// To save all attributes to be restored on z/OS and non-z/OS systems : tar -UX
				def processCmd = [
					"sh",
					"-c",
					"tar cUXf $tarFile *"
				]

				def processRC = runProcess(processCmd, tempLoadDir)
				rc = Math.max(rc, processRC)
				if (rc == 0) {
					println("** Package '${tarFile}' successfully created.")
				} else {
					println("*! [ERROR] Error when creating Package '${tarFile}' with rc=$rc.")
				}
			}
		}

		
		if (props.verbose && props.verbose.toBoolean() && rc  == 0) {
			println ("** List package contents.")

			processCmd = [
				"sh",
				"-c",
				"tar tvf $tarFile"
			]

			processRC = runProcess(processCmd, new File(props.workDir))
			rc = Math.max(rc, processRC)
			if (rc != 0) {
				println("*! [ERROR] Error when listing contents of Package '${tarFile}' with rc=$rc.")
			}
		}

		//Set up the artifact repository information to publish the tar file
		if (props.publish && props.publish.toBoolean() && rc == 0){
			// Configuring artifact repositoryHelper parms
			def url = artifactRepositoryHelpers.computeArchiveUrl(props)

			def apiKey = props.'artifactRepository.user'
			def user = props.'artifactRepository.user'
			def password = props.'artifactRepository.password'
			def httpClientVersion = props.'artifactRepository.httpClientVersion'
			def repo = props.get('artifactRepository.repo') as String
			println ("** Upload package to Artifact Repository '$url'.")
			rc = artifactRepositoryHelpers.upload(url, tarFile as String, user, password, props.verbose.toBoolean(), httpClientVersion)

			// generate PackageInfo for Concert Manifest file
			if (props.generateConcertBuildManifest && props.generateConcertBuildManifest.toBoolean()) {
				concertManifestGeneratorUtilities.addLibraryInfoTobuild(concertBuild, tarFileName, url)
			}
		}

		if (concertManifestGeneratorUtilities && props.generateConcertBuildManifest && props.generateConcertBuildManifest.toBoolean() && rc == 0) {
			// concert_build_manifest.yaml is the default name of the manifest file

			if (props.generateSBOM && props.generateSBOM.toBoolean() && rc == 0) {
				concertManifestGeneratorUtilities.addSBOMInfoToBuild(concertBuild, sbomFileName, sbomSerialNumber)
			}
			concertManifestGeneratorUtilities.writeBuildManifest(new File("$tempLoadDir/concert_build_manifest.yaml"), props.fileEncoding, props.verbose)
			println("** Add concert build config yaml to tar file at ${tarFile}")
			// Note: https://www.ibm.com/docs/en/zos/2.4.0?topic=scd-tar-manipulate-tar-archive-files-copy-back-up-file
			// To save all attributes to be restored on z/OS and non-z/OS systems : tar -UX
			def processCmd = [
				"sh",
				"-c",
				"tar rUXf $tarFile concert_build_manifest.yaml"
			]

			def processRC = runProcess(processCmd, tempLoadDir)
			rc = Math.max(rc, processRC)
			if (rc == 0) {
				println("** Package '${tarFile}' successfully appended with concert manifest yaml.")
			} else {
				println("*! [ERROR] Error appending '${tarFile}' with concert manifest yaml rc=$rc.")
			}
		}
	}
}

if (rc > 0) {
	println ("*! [ERROR] Packaging failed with rc=$rc. Review the console output.")
} else {
	println ("** Packaging completed successfully.")
}
System.exit(rc)

def copyArtifactsToUSS(Map<DeployableArtifact, Map> buildOutputsMap, String tarSubfolder, HashMap<String, String> copyModeMap) {
	buildOutputsMap.each { deployableArtifact, info ->
		String container = info.get("container")
		String owningApplication = info.get("owningApplication")
		Record record = info.get("record")
		PropertiesRecord propertiesRecord = info.get("propertiesRecord")
		DependencySetRecord dependencySetRecord = info.get("dependencySetRecord")

		def relativeFilePath = ""
		if (deployableArtifact.artifactType.equals("zFSFile")) {
			relativeFilePath = "$binSubfolder/uss"
		} else {
			if (deployableArtifact.deployType.equals("OBJ")) {
				relativeFilePath = "$tarSubfolder/bin"
			} else {
				relativeFilePath = "$tarSubfolder/${deployableArtifact.deployType.toLowerCase()}"
			}
		}

		// define file name in USS
		def fileName = deployableArtifact.file

		// add deployType to file name
		if (props.addExtension && props.addExtension.toBoolean()) {
			fileName = fileName + '.' + deployableArtifact.deployType
		}
		def file = new File("$tempLoadDir/$relativeFilePath/$fileName")

		def (directory, relativeFileName) = extractDirectoryAndFile(file.toPath().toString())
		new File(directory).mkdirs()


		if (deployableArtifact.artifactType.equals("zFSFile")) {
			def originalFile = new File(container + "/" + deployableArtifact.file)
			println "\tCopy '${originalFile.toPath()}' to '${file.toPath()}'"
			try {
				copyFiles(originalFile.toString(), file.toString())
				
				// Append record to Wazi Deploy Application Manifest
				if (wdManifestGeneratorUtilities && props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
					wdManifestGeneratorUtilities.appendArtifactToManifest(deployableArtifact, "$relativeFilePath/$fileName", record, dependencySetRecord, propertiesRecord)
				}
				
			} catch (IOException exception) {
				println "!* [ERROR] Copy failed: an error occurred when copying '${originalFile.toPath()}' to '${file.toPath()}'"
				rc = Math.max(rc, 1)
			}
		} else if  (deployableArtifact.artifactType.equals("DatasetMember")) {
			// set copyMode based on last level qualifier
			currentCopyMode = copyModeMap[container.replaceAll(/.*\.([^.]*)/, "\$1")]
			if (currentCopyMode != null) {
				if (ZFile.exists("//'$container(${deployableArtifact.file})'")) {
					// Copy outputs to HFS
					CopyToHFS copy = new CopyToHFS()
					copy.setCopyMode(DBBConstants.CopyMode.valueOf(currentCopyMode))
					copy.setDataset(container)

					println "\tCopy '$container(${deployableArtifact.file})' to '$tempLoadDir/$relativeFilePath/$fileName' with DBB Copymode '$currentCopyMode'"
					copy.dataset(container).member(deployableArtifact.file).file(file).execute()

					// Tagging binary files
					if (currentCopyMode == CopyMode.BINARY || currentCopyMode == CopyMode.LOAD) {
						StringBuffer stdout = new StringBuffer()
						StringBuffer stderr = new StringBuffer()
						Process process = "chtag -b $file".execute()
						process.waitForProcessOutput(stdout, stderr)
						if (stderr){
							println ("*! stderr : $stderr")
							println ("*! stdout : $stdout")
						}
					}

					// Append record to Wazi Deploy Application Manifest
					if (wdManifestGeneratorUtilities && props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
						wdManifestGeneratorUtilities.appendArtifactToManifest(deployableArtifact, "$relativeFilePath/$fileName", record, dependencySetRecord, propertiesRecord)
					}
				} else {
					println "*! [ERROR] Copy failed: The file '$container(${deployableArtifact.file})' doesn't exist."
					rc = Math.max(rc, 1)
				}
			} else {
				println "*! [ERROR] Copy failed: The file '$container(${deployableArtifact.file})' could not be copied due to missing mapping."
				rc = Math.max(rc, 1)
			}
		} else if  (deployableArtifact.artifactType.equals("DatasetMemberDelete")) {
			// generate delete instruction for Wazi Deploy
			if (wdManifestGeneratorUtilities && props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
				wdManifestGeneratorUtilities.appendArtifactDeletionToManifest(deployableArtifact, "$relativeFilePath/$fileName", record, propertiesRecord)
			}
		}

		if (props.generateSBOM && props.generateSBOM.toBoolean() && rc == 0) {
			sbomUtilities.addEntryToSBOM(deployableArtifact, info)
		}
	}
}

/**
 * parse data set name and member name
 * @param fullname e.g. BLD.LOAD(PGM1)
 * @return e.g. (BLD.LOAD, PGM1)
 */
def getDatasetName(String fullname){
	def ds,member;
	def elements =  fullname.split("[\\(\\)]");
	ds = elements[0];
	member = elements.size()>1? elements[1] : "";
	return [ds, member];
}

/**
 * Extract the directory and the file 
 * from the fullname of a zFS file
 * For instance: /var/test/file.txt  --> [/var/test, file.txt]
 */
def extractDirectoryAndFile(String fullname) {
	Path filePath = Paths.get(fullname);
	String file = filePath.getFileName().toString();
	String directory = filePath.getParent();
	return [directory, file];
}

/**
 * read cliArgs
 */
def parseInput(String[] cliArgs){
	def cli = new CliBuilder(usage: "PackageBuildOutputs.groovy [options]", stopAtNonOption:false)
	// required packaging options
	cli.w(longOpt:'workDir', args:1, argName:'dir', 'Absolute path to the DBB build output directory')
	cli.properties(longOpt:'packagingPropertiesFile', args:1, argName:'packagingPropertiesFile', 'Path of a property file containing application specific packaging details.')

	// optional packaging options
	cli.d(longOpt:'deployTypes', args:1, argName:'deployTypes','Comma-seperated list of deployTypes to filter on the scope of the tar file. (Optional)')
	cli.t(longOpt:'tarFileName', args:1, argName:'filename', 'Name of the package tar file. (Optional unless using --buildReportOrder or --buildReportOrderFile)')
	cli.il(longOpt:'includeLogs', args:1, argName:'includeLogs', 'Comma-separated list of file extensions matching files from the USS build workspace. (Optional)')
	cli.ae(longOpt:'addExtension', 'Flag to add the deploy type extension to the member in the package tar file. (Optional)')

	// Wazi Deploy Application Manifest generation
	cli.wd(longOpt:'generateWaziDeployAppManifest', 'Flag indicating to generate and add the Wazi Deploy Application Manifest file.')
	cli.bi(longOpt:'buildIdentifier', args:1, argName:'buildIdentifier', 'Unique build identifier stored in Wazi Deploy Application Manifest file.')
	cli.ed(longOpt:'externalDependenciesEvidences', args:1, argName:'externalDependenciesEvidences', 'File documenting the external dependencies that were provided to the build phase.')


	// Concert Build Manifest generation
	cli.ic(longOpt:'generateConcertBuildManifest', 'Flag indicating to generate and add the IBM Concert Build Manifest file.')

	cli.b(longOpt:'branch', args:1, argName:'branch', 'The git branch processed by the pipeline')
	cli.a(longOpt:'application', args:1, argName:'application', 'The name of the application')

	cli.af(longOpt:'applicationFolderPath', args:1, argName:'applicationFolderPath', 'Path to the Application\'s Git repository folder')

	// Baseline package
	cli.bp(longOpt:'baselinePackage', args:1, argName:'baselinePackageFilePath', 'Path to a baseline Package. (Optional)')

	// Artifact repository options ::
	cli.p(longOpt:'publish', 'Flag to indicate package upload to the provided Artifact Repository server. (Optional)')
	cli.v(longOpt:'versionName', args:1, argName:'versionName', 'Name of the version/package folder on the Artifact repository server. (Optional)')

	// Artifact repository info
	cli.au(longOpt:'artifactRepositoryUrl', args:1, argName:'url', 'URL to the Artifact repository server. (Optional)')
	cli.ar(longOpt:'artifactRepositoryName', args:1, argName:'repoName', 'Artifact repository name to store the build. (Optional)')
	cli.ad(longOpt:'artifactRepositoryDirectory', args:1, argName:'repoDirectory', 'Directory path in the repository to store the build . (Optional)')
	cli.aU(longOpt:'artifactRepositoryUser', args:1, argName:'user', 'User to connect to the Artifact repository server. (Optional)')
	cli.aP(longOpt:'artifactRepositoryPassword', args:1, argName:'password', 'Password to connect to the Artifact repository server. (Optional)')
	cli.ah(longOpt:'artifactRepositoryHttpClientProtocolVersion', args:1, argName: 'httpClientProtocolVersion', 'HttpClient.Version setting to override the HTTP protocol version. (Optional)')
	cli.aprop(longOpt:'artifactRepositoryPropertyFile', args:1, argName:'propertyFile', 'Path of a property file containing application specific artifact repository details. (Optional) ** (Deprecated)')

	// Tracing
	cli.verb(longOpt:'verbose', 'Flag to provide more log output. (Optional)')

	// multiple build reports
	cli.boFile(longOpt:'buildReportOrderFile', args:1, argName:'buildReportOrderFile', 'A file that lists build reports in order of processing')
	cli.bO(longOpt:'buildReportOrder', args:1, argName:'buildReportOrder', 'List of build reports in order of processing ')

	// SBOM generation
	cli.s(longOpt:'sbom', argName:'sbom', 'Flag to control the generation of SBOM')
	cli.sa(longOpt:'sbomAuthor', args:1, argName:'sbomAuthor', 'Author of the SBOM, in form "Name <email>"')

	// Owner of the artifacts
	cli.o(longOpt:'owner', args:1, argName:'owner', 'Owner of the packaged artifacts')

	cli.h(longOpt:'help', 'Prints this message')
	def opts = cli.parse(cliArgs)
	if (opts.h) {
		// if help option used, print usage and exit
		cli.usage()
		System.exit(0)
	}

	// read properties file
	if (opts.properties) {
		def propertiesFile = new File(opts.properties)
		if (propertiesFile.exists()) {
			props.packagingPropertiesFile = opts.properties
			propertiesFile.withInputStream { props.load(it) }
		}
	} else {
		// read default sample properties file shipped with the script
		def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
		def defaultPackagePropFile = new File("$scriptDir/packageBuildOutputs.properties")
		if (defaultPackagePropFile.exists()) {
			props.packagingPropertiesFile = "$scriptDir/packageBuildOutputs.properties"
			defaultPackagePropFile.withInputStream { props.load(it) }
		}
	}

	// set command line arguments
	if (opts.w) props.workDir = opts.w
	if (opts.d) props.deployTypeFilter = opts.d
	if (opts.t) props.tarFileName = opts.t
	if (opts.il) props.includeLogs = opts.il
	if (opts.a) props.application = opts.a
	if (opts.b) props.branch = opts.b
	if (opts.bi) props.buildIdentifier = opts.bi

	// cli overrides defaults set in 'packageBuildOutputs.properties'
	props.generateWaziDeployAppManifest = (opts.wd) ? 'true' : (props.generateWaziDeployAppManifest ? props.generateWaziDeployAppManifest : 'false')
	props.generateConcertBuildManifest = (opts.ic) ? 'true' : (props.generateConcertBuildManifest ? props.generateConcertBuildManifest : 'false')
	props.addExtension = (opts.ae) ? 'true' : (props.addExtension ? props.addExtension : 'true')
	props.publish = (opts.p) ? 'true' : (props.publish ? props.publish : 'false')
	props.generateSBOM = (opts.sbom) ? 'true' : (props.generateSBOM ? props.generateSBOM : 'false')
	props.publishInterfaces = (props.publishInterfaces ? props.publishInterfaces : 'false')

	if (opts.o) props.owner = opts.o

	props.verbose = (opts.verb) ? 'true' : 'false'

	if (opts.af) {
		props.applicationFolderPath = opts.af
		if (opts.bp) props.baselinePackageFilePath = opts.bp
	}

	// Track retrieved external dependencies
	if (opts.ed) props.externalDependenciesEvidences = opts.ed

	// default log encoding if not specified via config passed in via --properties
	if (!props.fileEncoding) props.fileEncoding = "IBM-1047"

	// Optional Artifact repository info to deploy package
	if (opts.v) props.versionName = opts.v

	// read of artifact repository file
	if (opts.aprop) {
		def propertyFile = new File(opts.aprop)
		if (propertyFile.exists()){
			propertyFile.withInputStream { props.load(it) }
		}
	}

	// read the artifact repo cli options, which take precedence over
	// the properties file

	if (opts.aU) props.'artifactRepository.user' = opts.aU
	if (opts.aP) props.'artifactRepository.password' = opts.aP
	if (opts.au) props.'artifactRepository.url' = opts.au
	if (opts.ar) props.'artifactRepository.repo' = opts.ar
	if (opts.ad) props.'artifactRepository.directory' = opts.ad
	if (opts.ah) props.'artifactRepository.httpClientVersion' = opts.ah

	//add any build reports from the file first, then add any from a CLI after.
	//if no file or CLI, go to default build report
	def buildReports = []
	if (opts.boFile) {
		new File (opts.boFile).eachLine { line ->
			buildReports.add(line)
		}

		if (opts.t == false) {
			println("*! [ERROR] Missing required property 'tarFilename'. 'tarFilename' is only optional when no build report order is specified.")
			rc = 2
		}
	}
	if (opts.bO) {
		opts.bO.split(',').each{
			buildReports.add(it)
		}
		if (opts.t == false) {
			println("*! [ERROR] Missing required property 'tarFilename'. 'tarFilename' is only optional when no build report order is specified.")
			rc = 2
		}
	} else if (buildReports.isEmpty()){
		buildReports = [
			props.workDir + "/BuildReport.json"
		]
	}
	props.buildReportOrder = buildReports

	if (opts.sbomAuthor) {
		props.sbomAuthor = opts.sbomAuthor
	}

	if (!props.workDir) {
		println("*! [ERROR] Missing required Working Directory parameter ('--workDir'). ")
		rc = 2
	}

	if (!props.copyModeMap) {
		println("*! [ERROR] Missing copyModeMap property in the configuration file.")
		rc = 2
	}

	// validate SBOM options
	if (props.generateSBOM && props.generateSBOM.toBoolean() && !props.application) {
		println("*! [ERROR] Missing Application ('-a') property required when generating SBOM.")
		rc = 2
	}

	// validate publishing options
	if (props.publish && props.publish.toBoolean()){
		if (!props.'artifactRepository.url') {
			println("*! [ERROR] Missing Artifact Repository URL property. It is required when publishing the package via ArtifactRepositoryHelpers.")
			rc = 2
		}
		if (!props.'artifactRepository.repo') {
			println("*! [ERROR] Missing Artifact Repository Name property. It is required when publishing the package via ArtifactRepositoryHelpers.")
			rc = 2
		}
		if (!props.'artifactRepository.user') {
			println("*! [ERROR] Missing Artifact Repository Username property. It is required when publishing the package via ArtifactRepositoryHelpers.")
			rc = 2
		}
		if (!props.'artifactRepository.password') {
			println("*! [ERROR] Missing Artifact Repository Password property. It is required when publishing the package via ArtifactRepositoryHelpers.")
			rc = 2
		}
	}

	// assess required options to generate Wazi Deploy application manifest
	if (props.generateWaziDeployAppManifest && props.generateWaziDeployAppManifest.toBoolean()) {
		if (!props.branch) {
			println("*! [ERROR] Missing Branch parameter ('--branch'). It is required for generating the Wazi Deploy Application Manifest file.")
			rc = 2
		}
		if (!props.addExtension || !props.addExtension.toBoolean()) {
			println("*! [ERROR] Missing AddExtension parameter ('--addExtension'). It is required for generating the Wazi Deploy Application Manifest file.")
			rc = 2
		}
	}

	// assess required options to generate Concert Build manifest
	if (props.generateConcertBuildManifest && props.generateConcertBuildManifest.toBoolean()) {
		if (!props.branch) {
			println("*! [ERROR] Missing Branch parameter ('--branch'). It is required for generating the Concert Build Manifest file.")
			rc = 2
		}
		if (!props.publish) {
			println("*! [ERROR] Missing Publish parameter ('--publish'). It is required for generating the Concert Build Manifest file.")
			rc = 2
		}
		if (opts.bO || opts.boFile) {
			println("*! [ERROR] Conflicting parameter ('-bO or -boFile'). IBM Concert Build Manifest file is created with single builds only.")
			rc = 2
		}
	}

	if (props.publishInterfaces && props.publishInterfaces.toBoolean()) {
		if (!props.applicationFolderPath) {
			println("*! [ERROR] Missing Application Folder Path parameter ('--applicationFolderPath'). It is required for publishing intefaces in the archive.")
			rc = 2
		}
	}

}

/*
 * relativizePath - converts an absolute path to a relative path from the workspace directory
 */
def relativizePath(String path) {
	if (!path.startsWith('/'))
		return path
	String relPath = new File(props.workDir).toURI().relativize(new File(path.trim()).toURI()).getPath()
	// Directories have '/' added to the end.  Lets remove it.
	if (relPath.endsWith('/'))
		relPath = relPath.take(relPath.length()-1)
	return relPath
}

/*
 * parseCopyModeMap - Parses the CopyModeMap provided in the PackageBuildOutputs.properties file, without using 'evaluate()'
 */
def parseCopyModeMap(String copyModeMapString) {
	HashMap<String, String> copyModeMap = new HashMap<String, String>()
	def copyModes = copyModeMapString.replaceAll("[\\[\\]\"]", "").trim().split(",")
	copyModes.each() { copyMode ->
		def copyModeParts = copyMode.split(":")
		copyModeMap.put(copyModeParts[0].trim(), copyModeParts[1].trim())
	}
	return copyModeMap
}


/*
 * checksBinaryInterfaces - Checks if all interfaces
 * (public/shared Include Files and submodules)
 * are present in the archive.
 * If not, issue a warning message
 */

def checkBinaryInterfaces(File tempLoadDir, applicationDescriptor) {
	ArrayList<String> binaryPublicInterfaces = applicationDescriptorUtils.getFilesByTypeAndUsage(applicationDescriptor, "Program", "service submodule")
	ArrayList<String> binaryPrivateInterfaces = applicationDescriptorUtils.getFilesByTypeAndUsage(applicationDescriptor, "Program", "internal submodule")
	binaryPublicInterfaces.each { binaryInterface ->
		String sourceFileName = Paths.get(binaryInterface).getFileName().toString()
		String member = sourceFileName.split("\\.")[0].toUpperCase()

		String expectedInterfaceFileName = "$includeSubfolder/bin/" + member + ".OBJ"
		File expectedInterfaceFile = new File("${tempLoadDir.getAbsolutePath()}/$expectedInterfaceFileName")
		if (!expectedInterfaceFile.exists()) {
			println("*! [WARNING] Public interface for $member not found at '$expectedInterfaceFileName'. Archive is not exposing all known interfaces.")
		}
	}
	binaryPrivateInterfaces.each { binaryInterface ->
		String sourceFileName = Paths.get(binaryInterface).getFileName().toString()
		String member = sourceFileName.split("\\.")[0].toUpperCase()

		String expectedInterfaceFileName = "$libSubfolder/bin/" + member + ".OBJ"
		File expectedInterfaceFile = new File("${tempLoadDir.getAbsolutePath()}/$expectedInterfaceFileName")
		if (!expectedInterfaceFile.exists()) {
			println("*! [WARNING] Private interface for $member not found at '$expectedInterfaceFileName'. Archive is not exposing all known interfaces.")
		}
	}
}

def retrieveBuildResultProperty(PropertiesRecord buildResultPropertiesRecord, String propertyName) {

	if (buildResultPropertiesRecord!=null) {
		buildResultProperties = buildResultPropertiesRecord.getProperties()

		def property = buildResultProperties.find {
			it.key.contains(propertyName)
		}

		if (property) {
			return property.getValue()
		} else {
			return null
		}
	}
}

/**
 * Copy files using the cp command, to preserve file tags
 */
def copyFiles(String sourceFile, String targetFile){
	StringBuffer resp = new StringBuffer()
	StringBuffer error = new StringBuffer()

	String cmd = "cp -Rf $sourceFile $targetFile"
	Process process = cmd.execute()
	process.waitForProcessOutput(resp, error)
	if (error) {
		String warningMsg = "*! Failed to execute shell command $cmd"
		println(warningMsg)
		println(error)
	}
}

/**
 * Execute a process
 */
def runProcess(ArrayList cmd, File dir){
	if (props.verbose && props.verbose.toBoolean()) println "   Executing $cmd "
	StringBuffer response = new StringBuffer()
	StringBuffer error = new StringBuffer()

	// execute cmd
	def p = cmd.execute(null, dir)

	p.waitForProcessOutput(response, error)
	if(response) println(response.toString())

	def rc = p.exitValue();
	if (rc != 0){
		println("*! [ERROR] Error executing $cmd \n" + error.toString())
	}
	return rc
}



/*
 * The DeployableArtifact class represent an artifact that can be deployed
 * It defines a file (typically the member name of a dataset (the container) or a file in a zFS directory)
 * and a deployType. Instances of this class are used in the main Map object to represent unique artifacts.
 */
class DeployableArtifact {
	private final String file;
	private final String deployType;
	private final String artifactType;

	DeployableArtifact(String file, String deployType, String artifactType) {
		this.file = file;
		this.deployType = deployType;
		this.artifactType = artifactType;
	}

	@Override
	public int hashCode() {
		String concatenation = file + "." + deployType + "." + artifactType;
		return concatenation.hashCode();
	}

	// Compares to DeployableArtifacts object and check if there are equal or not
	public boolean equals(DeployableArtifact other) {
		return other.file.equals(file) & other.deployType.equals(deployType) & (other.artifactType.equals(artifactType));
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DeployableArtifact) {
			return equals((DeployableArtifact) other)
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return file + "." + deployType;
	}
}
