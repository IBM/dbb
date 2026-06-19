# YAML Schema Validation

This directory contains tools for validating the `deployment-method.yml` file against its JSON schema.

## Prerequisites

### Python Virtual Environment

The VS Code tasks are configured to use the Python virtual environment at:
```
/Users/dennisbehm/devops-venv/bin/activate
```

Install the required Python packages in the virtual environment:

```bash
source /Users/dennisbehm/devops-venv/bin/activate
pip install pyyaml jsonschema
```

Or use the VS Code task: **"Install Python Dependencies for YAML Validation"** (automatically activates the venv)

## Validation Methods

### 1. VS Code Tasks (Recommended)

Three VS Code tasks are available in the Command Palette (`Cmd+Shift+P` or `Ctrl+Shift+P`):

#### Task: "Validate Deployment Method YAML Schema"
- **Usage**: Open any YAML file and run this task
- **Description**: Validates the currently open file against the schema
- **Best for**: Validating any YAML file in the workspace

#### Task: "Validate Deployment Method YAML (Fixed Path)"
- **Usage**: Run from anywhere in the workspace
- **Description**: Always validates `deployment-method.yml` specifically
- **Best for**: Quick validation of the main deployment method file

#### Task: "Install Python Dependencies for YAML Validation"
- **Usage**: Run once to install dependencies
- **Description**: Installs `pyyaml` and `jsonschema` packages

### 2. Command Line

Run the validation script directly with the virtual environment:

```bash
# Activate the virtual environment first
source /Users/dennisbehm/devops-venv/bin/activate

# From the deployment-configuration directory
python3 validate_schema.py \
    deployment-method.yml \
    ../../Schemas/dep-method-schema.json

# From the workspace root
python3 WaziDeploy/zDeploy/deployment-configuration/validate_schema.py \
    WaziDeploy/zDeploy/deployment-configuration/deployment-method.yml \
    WaziDeploy/Schemas/dep-method-schema.json
```

### 3. GitHub Actions / CI Pipeline

For automated validation in CI/CD pipelines, see the examples in the main repository documentation.

## Output Examples

### Successful Validation
```
======================================================================
YAML Schema Validation
======================================================================
✓ Loaded YAML file: deployment-method.yml
✓ Loaded schema file: ../../Schemas/dep-method-schema.json

✅ YAML validation successful!
   File: deployment-method.yml
   Schema: ../../Schemas/dep-method-schema.json
======================================================================
```

### Failed Validation
```
======================================================================
YAML Schema Validation
======================================================================
✓ Loaded YAML file: deployment-method.yml
✓ Loaded schema file: ../../Schemas/dep-method-schema.json

❌ Validation failed with 2 error(s):

Error 1:
  Location: metadata
  Message: 'name' is a required property
  Validator: required

Error 2:
  Location: activities → 0 → actions
  Message: [] is too short
  Validator: minItems

======================================================================
```

## Schema Reference

The validation uses the JSON Schema Draft 7 specification:
- **Schema Location**: `WaziDeploy/Schemas/dep-method-schema.json`
- **Schema URL**: https://github.com/IBM/dbb/raw/refs/heads/main/Wazi_Deploy/Schemas/dep-method-schema.json

## Troubleshooting

### Python Not Found
If you get a "python3: command not found" error:
- Install Python 3.7 or later
- On macOS: `brew install python3`
- On Linux: `sudo apt-get install python3`

### Missing Dependencies
If you get import errors, install packages in the virtual environment:
```bash
source /Users/dennisbehm/devops-venv/bin/activate
pip install pyyaml jsonschema
```

### Permission Denied
Make the script executable:
```bash
chmod +x validate_schema.py
```

## Integration with VS Code

The validation script is integrated with VS Code through tasks defined in `.vscode/tasks.json`. You can:

1. Run tasks via Command Palette: `Tasks: Run Task`
2. Bind tasks to keyboard shortcuts in `keybindings.json`
3. Add tasks to the status bar with extensions like "Task Runner"

## Additional Resources

- [JSON Schema Documentation](https://json-schema.org/)
- [Wazi Deploy Documentation](https://www.ibm.com/docs/en/wazi-deploy)
- [PyYAML Documentation](https://pyyaml.org/)
- [jsonschema Python Package](https://python-jsonschema.readthedocs.io/)