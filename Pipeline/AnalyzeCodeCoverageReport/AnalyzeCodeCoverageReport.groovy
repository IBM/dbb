@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import groovy.cli.commons.*

// The CCAPI.jar file must be present in the Java ClassPath
import com.ibm.debug.pdt.codecoverage.core.results.CCAbstractException;
import com.ibm.debug.pdt.codecoverage.core.results.CCResultException;
import com.ibm.debug.pdt.codecoverage.core.results.CCResultsFactory;
import com.ibm.debug.pdt.codecoverage.core.results.ICCModule;
import com.ibm.debug.pdt.codecoverage.core.results.ICCResult;

// run setup tasks
setup(args)

// process files passed through command-line
def String[] filesToProcess = files.split(',');

ICCResult results = null;
try {
	results = CCResultsFactory.getInstance().createResult(filesToProcess);
	ICCModule[] modules = results.getModules();
	println("** IBM DEBUG Code Coverage details");
	
	println("** Included modules:");
	for (ICCModule iccModule : modules) {
		println("\t " + iccModule.getName() + " - Code Coverage Percentage: " + iccModule.getPercentCoverage());
	}
	println("** Global Code Coverage Percentage: " + results.getPercentCoverage());
} catch (CCResultException e) {
	for (CCAbstractException ie : e.getExceptions())
		println(ie.getMessage());
	results = e.getResult();
	e.printStackTrace();
}

/* setup: handle cli arguments
*/
def setup(String[] args) {
	// parse input arguments
	String usage = 'AnalyzeCodeCoverageReport.groovy [options]'
	String header = 'options:'
	def cli = new CliBuilder(usage:usage,header:header)
	cli.f(longOpt:'files', args:1, 'Comma-separated list of Code Coverage CCZIP files to analyze')
	def opts = cli.parse(args)
	if (!args || !opts) {
		cli.usage()
		System.exit(1)
	}
	   
	if (opts.f) files = opts.f
	
	assert files : "Missing 'files' argument"
}
