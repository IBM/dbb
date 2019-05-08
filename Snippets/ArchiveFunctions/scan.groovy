// example call: groovyz scan.groovy /usr/PublicSamples/ Snippets/ArchiveFunctions/src/HELLOCBL.cbl
import com.ibm.dbb.build.*
import com.ibm.dbb.dependency.*

def sourceDir = args[0]
def file = args[1]

println("Source Directory: ${sourceDir}")
println("File: ${file}")

// scan tar file
def scanner = new DependencyScanner()
for (f in scanner.scanArchive(file, sourceDir)) {
	println(f)
}