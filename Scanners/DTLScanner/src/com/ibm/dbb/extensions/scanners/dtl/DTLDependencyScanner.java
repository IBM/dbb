package com.ibm.dbb.extensions.scanners.dtl;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.ibm.dbb.dependency.AbstractDependencyScanner;
import com.ibm.dbb.dependency.LogicalDependency;
import com.ibm.dbb.dependency.LogicalFile;

public class DTLDependencyScanner extends AbstractDependencyScanner {

	@Override
	protected LogicalFile createLogicalFile(String file, Object scanMetadata) {
		return (LogicalFile) scanMetadata;
	}

    private static Pattern entityPattern = Pattern.compile("<[:!][eE][nN][tT][iI][tT][yY]\\s%?(?<payload> ?[a-zA-Z0-9\\$@%]*)\\s[sS][yY][sS][tT][eE][mM].*>"); //$NON-NLS-1$
    private static Pattern inclPattern = Pattern.compile("<\\?(?<payload>.*)>"); //$NON-NLS-1$

	
	@Override
	protected Object runScan(String file, InputStream inputStream, String encoding) {

		LogicalFile lfile = new LogicalFile(createLogicalName(file), file, "DTL", false, false, false, false);
		
		String ext = file.substring(file.lastIndexOf('.')+1);
		
		// Set library type to DTLINC
		String library = "DTLINC";
		
		// Set a different library for Japanese dialogs includes
		if (ext.equals("dtljpn")) {
			library = "DTLINCJ";
		}
		// System.out.println("File is " + file + " ext is " + ext + " library is " + library);
		

		// make sure we use the right encoding
		Reader reader;
		try {
			reader = new InputStreamReader(inputStream, encoding);
			
	        BufferedReader br = new BufferedReader(reader);
	        
	        String line;
	        while ((line = br.readLine()) != null) {
	            
	        	//System.out.println("[DTL Dependency Scanner] reading : " + line);

	        	Matcher entityMatcher = entityPattern.matcher(line);
	        	Matcher inclMatcher = inclPattern.matcher(line);

	        	// We have found an <entity ... > match
	        	if (entityMatcher.find()) {
	        		// System.out.println(String.format("Found entity on line: %s\n", line));
		        	LogicalDependency logicalDependency = new LogicalDependency(entityMatcher.group("payload").trim().toUpperCase(), library, "COPY");
		        	lfile.addLogicalDependency(logicalDependency);
	        		
	        	}
	        	// We have found a <?include> match
	        	else if (inclMatcher.find()) {
	        		// System.out.println(String.format("Found include on line: %s\n", line));
		        	LogicalDependency logicalDependency = new LogicalDependency(inclMatcher.group("payload").trim(), library, "COPY");
		        	lfile.addLogicalDependency(logicalDependency);
	        	}
	        }			
			
		} catch (UnsupportedEncodingException e) {
			// logging=
			return lfile;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return lfile;
	}

}
