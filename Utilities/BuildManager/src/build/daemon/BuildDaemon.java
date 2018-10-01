package build.daemon;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.LinkedList;
import java.util.Properties;
import java.util.Queue;

import build.utils.ConfigurationException;

public class BuildDaemon {
	/**
     * Daemon server. Listens for commands manages Build Process objects.
     */
	Properties properties;
	BuildProcessInterface[] buildProcessPool;
	Queue<BuildCommand> buildCommandQueue = new LinkedList<>();
	
	// socket server properties
	ServerSocket serverSocket;
	
	// process queue properties
	String dbbHome;
	String javaHome;
	Integer processQueueLimit;
	
	
	public BuildDaemon(String filePath) throws ConfigurationException, IOException {
		// load properties
		this.properties = new Properties();
		try {
			properties.load(new FileInputStream(filePath));
		} catch (FileNotFoundException e) {
			System.out.println("*** Configuration file not found.");
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		this.verifyProperties();

		// load settings from properties
		this.processQueueLimit = Integer.parseInt(this.properties.getProperty("build_process.groovyz.preload.amount"));
		this.dbbHome = this.properties.getProperty("dbb_home").replace("\n", "");
		// Java Home is optional and can be null
		this.javaHome = this.properties.getProperty("java_home") == null ? "" : this.properties.getProperty("java_home").replace("\n", "");
		
		// Add leading path separator if needed
		if ( this.javaHome.length() > 0 && ! javaHome.endsWith(File.separator) )
			this.javaHome += File.separator;
		if ( this.dbbHome.length() > 0 && ! dbbHome.endsWith(File.separator) )
			this.dbbHome += File.separator;		
		
		// create socket
		System.out.println("*** SERVER START");
		this.serverSocket = new ServerSocket(Integer.parseInt(properties.getProperty("daemon_port")));
		

		// create queue
		System.out.println("*** BUILD PROCESS POOL CREATED");
		this.buildProcessPool = new BuildProcessInterface[this.processQueueLimit];
	}
	
	public void verifyProperties() throws ConfigurationException {
		// verify required properties exist
		if (properties.getProperty("dbb_home") == null) {
			throw new ConfigurationException("*** DBB_HOME needs to be set in the properties file.");
		}
		if (properties.getProperty("daemon_port") == null) {
			throw new ConfigurationException("*** DAEMON_PORT needs to be set in the properties file.");
		}
		
		// confirm proper port format
		try {
			Integer.parseInt(properties.getProperty("daemon_port"));
		} catch (NumberFormatException n) {
			n.printStackTrace();
		}
		
	}
	
	public void preload() throws IOException {
        for (int i = 0; i < processQueueLimit; i++) {
    			try {
				buildProcessPool[i] = new BuildProcessInterface("id_" + i, this.dbbHome, this.javaHome, 
						this.properties.getProperty("build_process.groovyz.preload.options"),
						this.properties.getProperty("build_process.groovyz.preload.classpath")
						);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
        }
        System.out.println("*** PROCESSES LOADED.");
    }
	
	public void produce(String command, DataOutputStream outboundConnection) {
		this.buildCommandQueue.add(new BuildCommand(command, outboundConnection));
	}
	
	public BuildCommand consume() { 
		return this.buildCommandQueue.poll();
	}
	

	
    public static void main(String[] args) throws IOException, ConfigurationException { 
    		BuildDaemon buildDaemon;
    		try {
    			buildDaemon = new BuildDaemon(args[0]);
    		}
    		catch (ArrayIndexOutOfBoundsException e) {
    			throw new ConfigurationException("*** Build Daemon requires a configuration file parameter");
    		}
    		
    		System.out.println("*** BUILD DAEMON START");
    		buildDaemon.preload();
        // handle messages from client
    		
    		// start queue thread
    		Thread command_thread = new Thread(){
    		    public void run(){
    		      while(true) {
    		    	  	try {
					Thread.sleep(1000);
					if (buildDaemon.buildProcessPool.length > 0) {
						for (int i = 0; i < buildDaemon.processQueueLimit; i++) {
		        				if (buildDaemon.buildProcessPool[i].status == 0) {
		        					BuildCommand buildCommand = buildDaemon.consume();
		    						if (buildCommand != null) {
			        					// send build command
				        				System.out.printf("*** Sending command to process: %s.", buildDaemon.buildProcessPool[i].identifier);	
				    	        			buildDaemon.buildProcessPool[i].sendCommand(buildCommand.command, buildCommand.outboundConnection);
				    	        			break;
		    						}
			        			}
						}
					}
				} catch (InterruptedException e) {
					break;
				}
    		      }
    		    }
    		  };

    		 command_thread.start();

    		
        while(true) {
        		System.out.println(buildDaemon.javaHome);
	        	Socket connectionSocket;
	        	BufferedReader inboundConnection;
	        	DataOutputStream outboundConnection;
	        	
            connectionSocket = buildDaemon.serverSocket.accept();
	    		inboundConnection = new BufferedReader(new InputStreamReader(connectionSocket.getInputStream()));
	        outboundConnection = new DataOutputStream(connectionSocket.getOutputStream());
	        
	        
            // read message
	        String inboundMessage;
	        try {
	        		inboundMessage = inboundConnection.readLine().replace("'", "");
	        } 
	        catch (NullPointerException e) {
	        		inboundMessage = "ERROR";
	        }
            
        
            System.out.println("COMMAND RECEIVED: " + inboundMessage);
            // check for command
            String COMMAND = inboundMessage.split(" ")[0];
            
            if (COMMAND.contains("kill")) {
            		try {
            			outboundConnection.writeBytes("*** Kill command received. \n");
            		} catch (SocketException e) {
            			System.out.println("*** FAILED TO WRITE TO CLIENT");
            		}
            		
				// send kill command, requires groovy script modification
				for (int i = 0; i < buildDaemon.processQueueLimit; i++) {
					System.out.printf("*** Stopping Groovy Process: %s \n", buildDaemon.buildProcessPool[i].identifier);
					buildDaemon.buildProcessPool[i].sendCommand("__STOP__", outboundConnection);
				}
				// shut down listener threads
				try {
					System.out.println("*** Shutting down process listener threads.");
					// wait for last listener to close
					buildDaemon.buildProcessPool[buildDaemon.processQueueLimit-1].listener.join();
				} catch (InterruptedException e) {
					e.printStackTrace();
				} 
				// shut down command thread
				try {
					System.out.println("*** Shutting down command thread.");
					command_thread.interrupt();
					command_thread.join();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				System.out.println("*** Shutting down command processor.");
				break;
				
            }
            else if (COMMAND.startsWith("groovy")){
            		// pass arguments as well
                String PROCESS_FILE = inboundMessage.split(" ", 2)[1];
	        		buildDaemon.produce(PROCESS_FILE, outboundConnection);
	        	}	
            else {
            		try {
            			outboundConnection.writeBytes("*** (SERVER) Unrecognized command received from client. \n");
            			
            		} catch (SocketException e) {
            			System.out.println("*** FAILED TO WRITE TO CLIENT");
            		}
            		outboundConnection.close();
            }
            
        }
        
        // close socket
        buildDaemon.serverSocket.close();
        System.out.println("*** DAEMON SHUTDOWN"); 
    }
}


