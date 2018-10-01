package build.daemon;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.SocketException;
import java.util.Scanner;

public class BuildProcessListener extends Thread{
	BuildProcessInterface process;
	DataOutputStream client;
	Boolean idle;
	
	public BuildProcessListener(BuildProcessInterface proc) {
		this.process = proc;
		this.idle = true;
	}
	
	public void setPipe(DataOutputStream client) {
		this.client = client;
		this.idle  = false;
	}

	public void run() {
		// connect to Groovy process input stream
		final Scanner in = new Scanner(process.process.getInputStream());
		String response;
	    while (true) {
	    		try {
	    				// listen for interrupt
					Thread.sleep(1);
					// idle check
					while(!this.idle) {
						// listen for interrupt
						Thread.sleep(1);
						// if not idle, read lines
			    			while (in.hasNextLine()) {
				    			try {
				    				// listen for interrupt
				    				Thread.sleep(1);
				    				// read from Groovy script
				    				response = in.nextLine();
				    				// if build is complete, update our status
						    		if (response.startsWith("__RC=")) {
						    			// strip message and send to client
						    			try  {
						    				System.out.printf("*** Ending execution: (PROCESS %s)\n", process.identifier);
						    				this.client.writeBytes(response.replace("_", "") + "\n");
						    			} catch (SocketException e) {
						    				System.out.println("*** FAILED TO WRITE TO CLIENT");
						    			}
						    			
						    			// update status
						    			process.status = 0;
						    			// set listener idle
						    			this.idle = true;
						    			// close client
						    			client.close();
						    			break;
						    		} 
						    		// if we need to kill process, return this thread and propogate up
						    		else if (response.contains("KILL")) {
						    			System.out.printf("(PROCESS %s) DBBListener closing \n", process.identifier);
						    			// close pipe
						    			in.close();
						    			return;
						    		}
						    		// debug for what the build script is doing
						    		else {
						    			this.client.writeBytes(response + "\n");
						    		}
				    			} catch (IOException e) {
								// TODO Auto-generated catch block
								e.printStackTrace(System.out);
							}
				    			
				    		}
			    		}
				} catch (InterruptedException e) {
					break;
				}
	    		
	    		
	    }
	    in.close();
	}
}