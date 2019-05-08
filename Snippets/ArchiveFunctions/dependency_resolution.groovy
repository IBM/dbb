// example call: groovyz dependency_resolution.groovy /usr/PublicSamples/ Snippets/ArchiveFunctions/src/HELLOCBL.cbl

import com.ibm.dbb.build.*
import com.ibm.dbb.dependency.*

def sourceDir = args[0] // e.g "/u/user/PublicSamples/Snippets/ArchiveFunctions/"
def file = args[1] // e.g "src/HELLOCBL.cbl"

println("Source Directory: ${sourceDir}")
println("File: ${file}")

// resolve dependency
def path = new DependencyPath().sourceDir(sourceDir).archive("archive/archive_copybook.tar").directory(sourceDir)
def rule = new ResolutionRule().library("SYSLIB").path(path)
def resolver = new DependencyResolver().sourceDir(sourceDir).file(file).rule(rule)
resolver.setScanner(new DependencyScanner())

println(resolver.resolve())

// resolve normally
path = new DependencyPath().sourceDir(sourceDir).directory("copybook/")
rule = new ResolutionRule().library("SYSLIB").path(path)
resolver = new DependencyResolver().sourceDir(sourceDir).file(file).rule(rule)
resolver.setScanner(new DependencyScanner())

println(resolver.resolve())
