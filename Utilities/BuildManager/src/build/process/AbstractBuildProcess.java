package build.process;

import java.security.Permission;
import java.util.Scanner;

/**
 * Reusable build process that keeps the JVM alive between process runs
 *
 */
public abstract class AbstractBuildProcess {

	protected boolean debug;
	
	/**
	 * Handles System.in communication from BuildDaemon and 
	 * calls runProcess to execute the build process.
	 * 
	 * Should be invoked by sub-class static main method
	 */
	public void run() {
		final Scanner in = new Scanner(System.in);
		String input;
		System.setSecurityManager(new CatchExitSecurityManager());
		debug("Starting "+this.getClass().getCanonicalName()+" . . .");
		
		while (true) {
			try {				
				Thread.sleep(250);
    			while (in.hasNextLine()) {
    				// read incoming command
	    			input = in.nextLine();
	    			debug("Input: "+input);
	    			
    				if (input.equals("__STOP__")) {
	    				// handle STOP command
	    				System.out.println("Stopping "+this.getClass().getCanonicalName()+" . . .");
	    				in.close();
	    				return;
	    			}
	    			else if (input.equals("__PING__")) {
	    				// handle PING command
	    				System.out.println("__RC=0__");
	    			}
	    			else {
	    				try {
	    					// run build process
	    					runProcess(input);
	    					System.out.println("__RC=0__");
	    				}
	    				catch (SecurityException se) {
	    					// handle build process system exit calls
	    					String message = se.getMessage();
	    					if (message != null && message.startsWith("__RC=")) 
	    						System.out.println(message);
	    					else {
	    						se.printStackTrace(System.out);
	    						System.out.println("__RC=-1__");
	    					}
	    				}
	    				catch (Exception e) {
	    					// handle all other exceptions
	    					e.printStackTrace(System.out);
	    					System.out.println("__RC=-1__");
	    				}
	    			}
	    		}
			}
			catch (Exception e) {
				e.printStackTrace();
			}
			finally {
				in.close();
			}
		}
	}
	
	/**
	 * Method that actually executes the build process.  Could be
	 * a Java process, a Groovy process, an Ant process etc.
	 * 
	 * Implemented by a process specific sub-class i.e. GroovyBuildProcess
	 */
	public abstract void runProcess(String processCommand) throws Exception;
	
	
	protected void debug(String message) {
		if (debug)
			System.out.println(message);
	}
	
	
	/**
	 * Custom SecurityManager to prevent build processes from terminating the JVM
	 *
	 */
	public class CatchExitSecurityManager extends SecurityManager {
		
		public CatchExitSecurityManager() {
			super();
		}
		
		// catch all System.exit calls and throw a SecurityException with the RC in the message
		public void checkExit(int status) {
			throw new SecurityException("__RC="+status+"__");
		}
		
        @Override
        public void checkPermission(Permission perm) {
            return; 
        }

        @Override
        public void checkPermission(Permission perm, Object context) {
            return; 
        }
	}
	
}
