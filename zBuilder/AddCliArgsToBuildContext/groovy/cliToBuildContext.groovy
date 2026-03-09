@groovy.transform.BaseScript com.ibm.dbb.groovy.TaskScript baseScript
import com.ibm.dbb.task.TaskConstants

println "   > Add CLI args to Build Context"

// See https://www.ibm.com/docs/en/adffz/dbb/3.0.x?topic=tutorials-creating-custom-task
// https://www.ibm.com/docs/api/v1/content/SS6T76_3.0.0/javadoc/com/ibm/dbb/task/AbstractTask.html#getCommandLine()

// retrieve command line
def cli = context.getCommandLine(TaskConstants.COMMAND_LINE)

cli.getOptions().each (opt -> {
	String name = opt.getLongOpt() != null ? opt.getLongOpt() : opt.getOpt();
	String value = opt.hasArg() ? opt.getValue() : "true";
	log.debug("cliToBuildContext.groovy loaded cli into build context:", name + "=" + value);
    // add key + value to build context for subsequent processing
	context.setVariable(name, value)
});

return 0