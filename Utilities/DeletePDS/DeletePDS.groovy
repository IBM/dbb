@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import groovy.transform.*
import groovy.cli.commons.*
import com.ibm.jzos.CatalogSearch;
import com.ibm.jzos.CatalogSearchField;
import com.ibm.jzos.ZFile;


@Field String hlq
@Field String filter
@Field Boolean preview = false

// run setup tasks
setup(args)


// process datasets with provided HLQ
filter = hlq
if (!filter.endsWith(".**"))
	filter = filter + ".**"
if (preview)
	println("** Searching for all the datasets filtered with HLQ '${hlq}'")
else
	println("** Deleting all datasets filtered with HLQ '${hlq}'")


CatalogSearch catalogSearch = new CatalogSearch(filter, 64000);
catalogSearch.addFieldName("ENTNAME");
catalogSearch.search();
def datasetCount = 0

catalogSearch.each { searchEntry ->
	CatalogSearch.Entry catalogEntry = (CatalogSearch.Entry) searchEntry;
	if (catalogEntry.isDatasetEntry()) {
		datasetCount++;
		CatalogSearchField field = catalogEntry.getField("ENTNAME");
		String dsn = field.getFString().trim();
		String qdsn = "'" + dsn + "'";	//Specify that the dsn is fully qualified.
		if (ZFile.dsExists(qdsn)) {
			if (preview) {
				println "*** Found $qdsn"	
			} else {
				println "*** Deleting $qdsn"
				ZFile.remove("//$qdsn")
			}
		 }
	}
}
if (preview)
	println("** Found $datasetCount entries.")
else
	println("** Deleted $datasetCount entries.")
	

/*
 setup :
 handle cli arguments
*/
def setup(String[] args) {
	// parse input arguments
	String usage = 'DeletePDS.groovy [options]'
	String header = 'options:'
	def cli = new CliBuilder(usage:usage,header:header)
	cli.h(longOpt:'hlq', args:1, 'High-Level Qualifier of datasets to delete')
	cli.p(longOpt:'preview', 'Only lists the datasets without actually deleting them')
	def opts = cli.parse(args)
	if (!args || !opts) {
		cli.usage()
		System.exit(1)
	}
	   
	// update authentication properties with cli options
	if (opts.h) hlq = opts.h
	if (opts.p) preview = opts.p
	
	assert hlq : "Missing 'hlq' argument"
}