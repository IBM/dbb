# JCL to zBuilder Language Yaml Conversion

## Usage
```
bin/JCLtoYAML.sh [options]
```

### Required Options:
- `-d, --dataset <MVS dataset>`: The PDS containing the JCL member to migrate.
- `-m, --member <JCL member>`: The JCL member to migrate.

### Optional Options:
- `-o,--outputDir <output directory>`: Directory in the HFS where all files will be written.
- `-c,--configFolder <configFolder>`: Path to the configuration folder ccontaining the JCL migration configuration file and dataset mappings configuration file. The default is `jclMigration/`.

### conf/jclmig.conf Properties:
- `restrictedPgms`: A comma seperated list of restricted programs. Upon encountering one of these programs, the JCL call will be extracted and placed as a JobExec step into the generated yaml. As opposed to a MVSExec step being used as normal.
- `tempDatasetOptions`: The BPXWDYN options for temporary allocated datasets, this defaults to `cyl space(5,5) unit(vio) blksize(80) lrecl(80) recfm(f,b) new`.
- `procLibs`: A comma seperated list of libraries that will be scanned to located JCL procs.

### conf/datasetMappings.properties Properties:
This properties file contains a mapping of LLQs to zBuilder dataset variables. Upon finding an LLQ matching the key in this properties file, the migration utility will replace the entire dataset name with the variable and place a variable definition for the dataset at the top of the file. These definitions normally live in the `Languages.yaml` sample, and may be relocated there if desired.

These definitions are primarily used for common load libraries.

### Migration Steps:
- Ensure the JCL file is uploaded to a dataset for migration. It must be executed JCL, a standalone proc cannot be migrated.
- Execute the `bin/JCLtoYAML.sh` shell script.
  - After execution, the output folder will contain a UTF-8 encoded `<JCL member>.yaml` file

#### Paramaterizing the generated Language YAML:
- Replace the value `<LANG_NAME>` in the `tasks[0]:language` field with the desired language name. This value is used to associate a task with a lifecycle
- Replace the values in the `tasks[0]:sources` field with source file patterns that match the files this JCL is intended to build.
- Replace the values in the `tasks[0]:datasets` field with datasets and their associated options which should be created by the zBuilder during execution.
  - These datasets are commonly used to hold source files, their dependencies, and their compilation output.
  - The format generally used in shipped language samples is: `${HLQ}.COBOL`, `${HLQ}.COPY`, `${HLQ}.LOAD`
  - Ensure the values used in the `dds:` section of a step are updated to match these values if they are generated at runtime.
    - The source dataset used in the step DD statements will generally be parameterized as such: `${HLQ}.COBOL(${MEMBER})` where member is a provided variable matching the name of the source file
- Commonly used datasets will be pulled into a `tasks[0]:variables:` section
  - These variable definitions match the placeholders in the `samples/languages/Languages.yaml` file and may be relocated to this file if wanted.
