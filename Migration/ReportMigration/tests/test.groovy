import groovy.lang.GroovyClassLoader
import groovy.junit5.plugin.JUnit5Runner
import groovy.cli.commons.CliBuilder

CliBuilder parser = new CliBuilder(usage:"\$DBB_HOME/bin/groovyz test.groovy --id CLIENT-ID --url CLIENT-URL --pwFile CLIENT-PASSWORD-FILE [--help]")
parser.url(type:String, longOpt:'url', args:1, required:true, 'Test Repository Client URL.')
parser.id(type:String, longOpt:'id', args:1, required:true, 'Test Repository Client user id.')
parser.pwFile(type:File, longOpt:'pwFile', args:1, required:true, 'Test Repository Client user password file.')
parser.help(longOpt:"help", "Prints this message.")

def options = parser.parse(args)
if (options == null) return;
if (options.help) {
    parser.usage()
    return
}

System.setProperty("test-url", options.url)
System.setProperty("test-id", options.id)
System.setProperty("test-pwFile", options.pwFile as String)

// Cant figure out how to collect classes into a single test suite like the JUnit 4 style
// A Nested central test class is the workaround
GroovyClassLoader cloader = new GroovyClassLoader(Thread.currentThread().getContextClassLoader())
File testDir = new File(getClass().getProtectionDomain().getCodeSource().getLocation().getPath()).getParentFile()
new JUnit5Runner().run(cloader.parseClass(new File(testDir, "StaticReportMigrationTests.groovy")), cloader)