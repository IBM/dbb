# Samples/API_Examples Folder
The API_Examples folder contains short scripts demonstrating how to use DBB APIs that might not be found in the other Sample applications. 

### ISPFExec/TSOExec Interactive Gateway Support Example (DBB v1.0.2 or higher)
DBB v1.0 introduced the ISPFExec and TSOExec commands that allows users to run ISPF and TSO commands during their build process.  The DBB toolkit achieves this by using the [Legacy TSO/ISPF Client Gateway](https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.3.0/com.ibm.zos.v2r3.f54pc00/isppctsoclit.htm).  Beginning in DBB v1.0.2, users now have the option to use the [Interactive TSO/ISPF Client Gateway](https://www.ibm.com/support/knowledgecenter/en/SSLTBW_2.3.0/com.ibm.zos.v2r3.f54pc00/isppccea.htm).

Advantages of using the Interactive ISPF Gateway:
* There is support for callers to execute programs that are interactive, using a conversational pattern.
* The TSO/E address spaces are started by using a TSO logon procedure. Sites can choose to use their existing TSO logon procedures for TSO/E address spaces started through the gateway.
* The Interactive ISPF Gateway uses z/OS Common Event Adapter (CEA) TSO/E address space services to start and manage TSO/E address spaces. The z/OS CEA TSO/E address space services provide support for the reuse of TSO/E address spaces, improving performance when a single TSO/E address space is used to issue multiple TSO or ISPF commands.


The sample is comprised of two files:
* InteractiveGateway.groovy : Script demonstrating DBB support for the Interactive ISPF Gateway
* gateway.properties : Configuration file that must be updated for the InteractiveGateway.groovy script to run.



