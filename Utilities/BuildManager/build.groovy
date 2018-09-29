import groovy.transform.Field
import java.nio.file.*

Properties properties = new Properties()
File propertiesFile = new File('resources/config.properties')
propertiesFile.withInputStream {
	properties.load(it)
}

assert properties.dbb_home

println("Building JAR for project...")
println("Creating temporary directory...")
def tempBinDir = new File("bin/")
if (tempBinDir.exists())
		tempBinDir.deleteDir()
	tempBinDir.mkdirs()
		
def ant = new AntBuilder()

def classpath = ant.path
{
   fileset(dir: "$properties.dbb_home/lib")
   {
	 include(name: "*.jar")
  }
  fileset(dir: "$properties.dbb_home/groovy-2.4.12/lib/")
  {
	  include(name: "*.jar")
  }
}

ant.javac(srcdir: "src", destdir: "$tempBinDir", classpath: "$classpath", encoding: 'UTF-8', includeantruntime: 'false', debug: 'on', debuglevel: 'lines,vars,source')
ant.copy(todir: tempBinDir)
{
		fileset(dir: "src", excludes: "**/*.java")
}

def jarFile = "dbb.manager.jar"
	ant.jar(destfile: "resources/$jarFile", basedir: tempBinDir)
	
	tempBinDir.deleteDir()
	println("Successfuly built JAR for project")
