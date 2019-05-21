import com.ibm.dbb.build.*
import com.ibm.dbb.dependency.*

def sourceDir = null
def file = null

try {
	sourceDir = args[0] // e.g "/u/user/PublicSamples/"
	file = args[1] // e.g "Snippets/ArchiveFunctions/src/HELLOCBL.cbl"
}
catch (Exception e){
	println("Correct usage: groovyz dependency_resolution.groovy {sourceDir} {file}")
	return
}


println("Source Directory: ${sourceDir}")
println("File: ${file}")

// resolve dependency
def path = new DependencyPath().sourceDir(sourceDir).archive("Snippets/ArchiveFunctions/archive/archive_copybook.tar").directory("copybook")
def rule = new ResolutionRule().library("SYSLIB").path(path)
def resolver = new DependencyResolver().sourceDir(sourceDir).file(file).rule(rule)
resolver.setScanner(new DependencyScanner())

println(resolver.resolve())

// resolve normally
path = new DependencyPath().sourceDir(sourceDir).directory("copybook")
rule = new ResolutionRule().library("SYSLIB").path(path)
resolver = new DependencyResolver().sourceDir(sourceDir).file(file).rule(rule)
resolver.setScanner(new DependencyScanner())

println(resolver.resolve())
