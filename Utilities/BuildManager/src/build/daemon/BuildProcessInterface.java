package build.daemon;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.PrintWriter;

public class BuildProcessInterface {
	// lookup information
	String type = "GroovyBuildProcess";
	// the script running
	Process process;
	// listener for script
	BuildProcessListener listener;
	// status of script
	Integer status;
	// id for script, testing purposes
	String identifier;
	// listener for commands
	PrintWriter commandListener;
	
	public BuildProcessInterface(String identifier, String DBB_HOME, String JAVA_HOME, String OPTIONS, String CLASSPATH) throws IOException, InterruptedException {
		if (JAVA_HOME.length() > 0) {
			JAVA_HOME += "bin/";
		} 
		// build the build process interface
		String command = "" +
							JAVA_HOME + "java" 
							+ " -cp ";

		// add classpath
		command += DBB_HOME + "lib/*:" + DBB_HOME +"groovy-2.4.12/lib/*";
		if (!CLASSPATH.isEmpty()) {
			command += ':' + CLASSPATH;
		}
		
		// add options
		if (!OPTIONS.isEmpty()) {
			command += " " + OPTIONS.replace("${id}", identifier);
		}
		
		// add extras
		command += ""
				+ " -Djava.library.path=" + DBB_HOME + "lib/" 
				+ " build.process.GroovyBuildProcess";
		
		// build process
		ProcessBuilder pb = new ProcessBuilder(
				command.split(" ")
				);
		pb.redirectErrorStream(true);
	    
		// start Groovy process
		this.process = pb.start();	
		
		// build the Groovy listener
		listener = new BuildProcessListener(this);
		// start the Groovy listener
		this.listener.start();
		
		// build the command listener
		commandListener = new PrintWriter(process.getOutputStream());
		
		// store variables
		this.status = 0;
		this.identifier = identifier;
		
		System.out.printf("*** PROCESS CREATED: %s \n", this.identifier);
	}
	
	public Integer getStatus() {
		return this.status;
	}
	
	public void setStatus(Integer status) {
		this.status = status;
	}
	
	public void sendCommand(String command, DataOutputStream client) {
		// update the Daemon Listener
		listener.setPipe(client);
		
		// send the command to the command listener
		commandListener.println(command);
		commandListener.flush();
		
		// update state
		status = 1;
		if (command.contains("__PING__")) {
			System.out.printf("*** (Process %s) Ping command sent. \n", this.identifier);
		}
		else if (command.contains("__STOP__")) {
			System.out.printf("*** (Process %s) Stop command sent. \n", this.identifier);
			this.listener.interrupt();
		}
		else {
			System.out.printf("*** (Process %s) Build command sent. \n", this.identifier);
		}
		
	}

}
