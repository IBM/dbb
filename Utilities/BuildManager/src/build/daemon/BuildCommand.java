package build.daemon;

import java.io.DataOutputStream;

public class BuildCommand {
	DataOutputStream outboundConnection;
	String command;
	
	public BuildCommand(String command, DataOutputStream outboundConnection) {
		this.command = command;
		this.outboundConnection = outboundConnection;
	}
}
