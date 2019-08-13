/*******************************************************************************
 * Licensed Materials - Property of IBM
 * (c) Copyright IBM Corporation 2019. All Rights Reserved.
 *
 * Note to U.S. Government Users Restricted Rights:
 * Use, duplication or disclosure restricted by GSA ADP Schedule
 * Contract with IBM Corp.
 *******************************************************************************/
import com.ibm.zoautil.*
import com.ibm.zoautil.types.*

def cli = new CliBuilder(usage: 'merge.groovy')
cli.h(longOpt:'help', argName:'hlq', 'Merge Sample')
cli.q(longOpt:'hlq', args:1, argName:'hlq', 'High level data set qualifier. Defaults to BUILDER.')
cli.m(longOpt:'mlq', args:1, argName:'mlq', 'Mid level data set qualifier. Defaults to DFSORT.')
def parameters = cli.parse(args)
if (parameters.h)
{
	cli.usage()
	System.exit(2)
}
hlq = ((parameters.q)?"${parameters.q}":"BUILDER").toUpperCase()
mlq = ((parameters.m)?"${parameters.m}":"DFSORT").toUpperCase()
dsPrefix = "${hlq}.${mlq}"
println "dataset prefix: $dsPrefix"

// Options to create a fixed block 80 sequential data set
// maxRC of 8 ignores an error if the dataset already exists
createDSOptions = new DatasetOptions().type(DatasetType.SEQ).recordFormat(RecordFormat.FB).lrecl(80).maxRC(8)

// Create Master dataset
Datasets.create("${dsPrefix}.master", createDSOptions)
Datasets.write("${dsPrefix}.master", "Charles      Field       278 323 6045")       // write
Datasets.write("${dsPrefix}.master", "David        George      397 132 6025", true) // append
Datasets.write("${dsPrefix}.master", "William      Young       178 333 5045", true) // append

// Create New dataset
Datasets.create("${dsPrefix}.new", createDSOptions)
Datasets.write("${dsPrefix}.new", "Emma         Hill        149 589 5045")       // write
Datasets.write("${dsPrefix}.new", "Sharon       Miller      153 232 6045", true) // append
Datasets.write("${dsPrefix}.new", "Steve        Green       748 111 6025", true) // append

// Create Cmd dataset
Datasets.create("${dsPrefix}.cmd", createDSOptions)
Datasets.write("${dsPrefix}.cmd", " MERGE FORMAT=CH,FIELDS=(1,9,A)") // write

// Create Merge dataset
Datasets.create("${dsPrefix}.merge", createDSOptions)


// Create DD Statements for sort
ddStatements = []
ddStatements << new DDStatement().ddName("sortin01").dataset("${dsPrefix}.master")
ddStatements << new DDStatement().ddName("sortin02").dataset("${dsPrefix}.new")
ddStatements << new DDStatement().ddName("sysin").dataset("${dsPrefix}.cmd")
ddStatements << new DDStatement().ddName("sortout").dataset("${dsPrefix}.merge")
ddStatements << new DDStatement().ddName("sysout").dataset("*")

// Sort
rc = MVSCmd.execute("sort","MSGPRT=CRITICAL,LIST",ddStatements);
println "MVSCmd rc: $rc"
if (rc == 0)
{
	result = Datasets.read("${dsPrefix}.merge")
	println "Merged dataset contents:\n$result"
}