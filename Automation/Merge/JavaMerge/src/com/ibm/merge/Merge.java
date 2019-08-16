package com.ibm.merge;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.ibm.zoautil.DatasetOptions;
import com.ibm.zoautil.DatasetType;
import com.ibm.zoautil.Datasets;
import com.ibm.zoautil.MVSCmd;
import com.ibm.zoautil.RecordFormat;
import com.ibm.zoautil.types.DDStatement;

public class Merge
{

	public static void main(String[] args) throws IOException
	{
		String hlq = "BUILDER";
		String mlq = "DFSORT";

		// Check parameters
		if ( args.length >= 1 )
		{
			if ( args[0].equals( "-?" ) || args[0].equals( "-h" ) )
			{
				System.out.println( "Run ZOAU Merge Java API example" );
				System.out.println( "\tjava com.ibm.merge.Merge <hlq or -?> <mlq>" );
				System.out.println( "\t\thlq - High level qualifier for datasets. Defaults to BUILDER." );
				System.out.println( "\t\tmlq - Mid level data set qualifier for datasets. Defaults to DFSORT." );
				System.exit( 0 );
			}
			else
			{
				hlq = args[0].toUpperCase();
			}
		}
		if ( args.length >= 2 )
		{
			mlq = args[1].toUpperCase();
		}

		String dsPrefix = hlq + "." + mlq;
		System.out.println( "dataset prefix: " + dsPrefix );

		String dsMaster = dsPrefix + ".master";
		String dsNew = dsPrefix + ".new";
		String dsCmd = dsPrefix + ".cmd";
		String dsMerge = dsPrefix + ".merge";

		// Options to create a fixed block 80 sequential data set
		// maxRC of 8 ignores an error if the dataset already exists
		DatasetOptions createDSOptions = new DatasetOptions().type( DatasetType.SEQ ).recordFormat( RecordFormat.FB ).lrecl( 80 ).maxRC( 8 );

		// Create Master dataset
		Datasets.create( dsMaster, createDSOptions );
		Datasets.write( dsMaster, "Charles      Field       278 323 6045" ); // write
		Datasets.write( dsMaster, "David        George      397 132 6025", true ); // append
		Datasets.write( dsMaster, "William      Young       178 333 5045", true ); // append

		// Create New dataset
		Datasets.create( dsNew, createDSOptions );
		Datasets.write( dsNew, "Emma         Hill        149 589 5045" ); // write
		Datasets.write( dsNew, "Sharon       Miller      153 232 6045", true ); // append
		Datasets.write( dsNew, "Steve        Green       748 111 6025", true ); // append

		// Create Cmd dataset
		Datasets.create( dsCmd, createDSOptions );
		Datasets.write( dsCmd, " MERGE FORMAT=CH,FIELDS=(1,9,A)" ); // write

		// Create Merge dataset
		Datasets.create( dsMerge, createDSOptions );

		// Create DD Statements for sort
		List<DDStatement> ddStatements = new ArrayList<DDStatement>();
		ddStatements.add( new DDStatement().ddName( "sortin01" ).dataset( dsMaster ) );
		ddStatements.add( new DDStatement().ddName( "sortin02" ).dataset( dsNew ) );
		ddStatements.add( new DDStatement().ddName( "sysin" ).dataset( dsCmd ) );
		ddStatements.add( new DDStatement().ddName( "sortout" ).dataset( dsMerge ) );
		ddStatements.add( new DDStatement().ddName( "sysout" ).dataset( "*" ) );

		// Sort
		int rc = MVSCmd.execute( "sort", "MSGPRT=CRITICAL,LIST", ddStatements );
		System.out.println( "MVSCmd rc: " + rc );
		if ( rc == 0 )
		{
			String results = Datasets.read( dsMerge );
			System.out.println( "Merged dataset contents:\n" + results );
		}
	}

}
