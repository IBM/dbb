package build.process;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;

import org.codehaus.groovy.control.CompilationFailedException;

import com.ibm.dbb.StaticMemoryReset;
import com.ibm.dbb.groovy.ScriptLoader;

import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyObject;

/**
 * Reusable Groovy build process that keeps the JVM alive between process runs
 *
 */
public class GroovyBuildProcess extends AbstractBuildProcess {

	private Map<String,Map<String,Object>> scriptCache;

	/**
	 * Constructor
	 */
	public GroovyBuildProcess() {
		super();
		scriptCache = new HashMap<String,Map<String,Object>>();
	}
	
	/**
	 * Parses and runs a DBB Groovy script
	 * 
	 * @param - Command string from Build Daemon
	 * @throws IOException 
	 * @throws IllegalAccessException 
	 * @throws InstantiationException 
	 * @throws CompilationFailedException 
	 */
	@Override
	public void runProcess(String processCommand) throws CompilationFailedException, InstantiationException, IllegalAccessException, IOException {
		debug("+GroovyBuildProcess.runProcess("+processCommand+")");
		
		// reset DBB static variables
		StaticMemoryReset.reset();
		
		// purge script cache of obsolete scripts
		purgeScriptCache();
		
		// get Groovy file from command
		StringTokenizer t = new StringTokenizer(processCommand);
		String groovyFile = (t.hasMoreTokens()) ? t.nextToken() : null;
		File file = new File(groovyFile);
		debug("Groovy file = "+file);
		if (!file.exists())
			throw new IOException("File "+file+" does not exist");
		// create Groovy arg array from command
		List<String> commandArgList = new ArrayList<String>();
		while (t.hasMoreTokens()) {
			commandArgList.add(t.nextToken());
		}
		String [] commandArgs = new String [commandArgList.size()];
		commandArgList.toArray(commandArgs);
		debug("args = "+Arrays.toString(commandArgs));
		
		// load the Groovy script and execute
		
		GroovyObject script = loadScript(file);
		
		debug("Executing script . . ");
		if (script instanceof ScriptLoader)
			script.invokeMethod("_run", commandArgs);
		else
			script.invokeMethod("main", commandArgs);
	}
	
	/*
	 * Loads an instance of a Groovy script from either the cache of parses the script and adds to the cache.
	 * 
	 */
	@SuppressWarnings({ "resource", "rawtypes" })
	private GroovyObject loadScript(File script) throws IOException, IllegalAccessException, InstantiationException {
		String scriptLocation = script.getPath();
		debug("Loading script for " + scriptLocation);
		
		if (scriptCache.get(scriptLocation) == null ) {
			debug("Script '" + scriptLocation + "' does not exist in cache. Parsing and adding to cache.");
			File sourceFile = new File(scriptLocation);
			Long lastModified = new Long(sourceFile.lastModified());
			Class groovyClass = new GroovyClassLoader().parseClass(sourceFile);
			Map<String,Object> scriptInfo = new HashMap<String,Object>();
			scriptInfo.put("class", groovyClass);
			scriptInfo.put("lastModified", lastModified);
			scriptCache.put(scriptLocation, scriptInfo);
		}

		GroovyObject inst = (GroovyObject) ((Class) scriptCache.get(scriptLocation).get("class")).newInstance();
		if (inst instanceof ScriptLoader) {
			((ScriptLoader) inst)._setScripts(scriptCache);
		}
		return (GroovyObject) inst;
	}
	
	/*
	 * This method iterates through the script cache verifying that the cached scripts are current.  If
	 * not then the cached scripts are deleted.  Newer scripts will be loaded when needed during the build
	 * process.
	 */
	private void purgeScriptCache() throws IOException {
		debug("Purging script cache. . .");
		Iterator<Entry<String,Map<String,Object>>> it = scriptCache.entrySet().iterator();
		while (it.hasNext()) {
			Entry<String,Map<String,Object>> entry = it.next();
			File sourceFile = new File(entry.getKey());
			if (sourceFile.exists()) {
				long lastModified = sourceFile.lastModified();
				Map<String,Object> value = entry.getValue();
				if (value == null) {
					debug("Script '" + entry.getKey() + "' has a null value.  Purging cache entry.");
					it.remove();					
				}
				Long cachedLastModified = (Long) value.get("lastModified");
				if (cachedLastModified == null) {
					debug("Script '" + entry.getKey() + "' cached lastModified value is null.  Purging cache entry.");
					it.remove();
				}
				else if (cachedLastModified.longValue() != lastModified) {
					debug("Cached script '" + entry.getKey() + "' is out of date with file system. " + cachedLastModified.longValue() + "!=" + lastModified  + " Purging cache entry.");
					it.remove();
				}
			}
			else {
				debug("Script '" + entry.getKey() + "' does not exist.  Purging cache entry.");
				it.remove();
			}
		}
	}
	
	
	/**
	 * Main method used to start the reusable build process
	 * @param args - Start up arguments i.e. -debug
	 */
	public static void main(String [] args) {
		GroovyBuildProcess process = new GroovyBuildProcess();
		if (args.length > 0 && args[0].equals("-debug"))
			process.debug = true;
		process.run();
	}

}
