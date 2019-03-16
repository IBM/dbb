# Run MVSJob in Multi-Thread Environment
The following sample shows how to modify a simple Compile.groovy to support multi-thread environment.

## Files
1. COBOL/*.cbl - Sample cobol files that are used by the compile groovy scripts.
2. Compile.groovy - Simple compile script that processes file in sequential. This script is used for comparison as well as a way to illustrate the steps required to convert to run MVSJob in multi-thread environment.
3. CompileUseThread.groovy - The version of compile groovy script that runs MVSJob in multi-thread environment.
4. *.sh - Shell scripts for convenienly running the compile scripts.
5. ElapsedTimer.groovy - A utility class to calculate time spent in each build to illustrate the benefit of running MVSJob in multi-thread environment.
