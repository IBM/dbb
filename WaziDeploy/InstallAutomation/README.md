# Installing Wazi Deploy with an Ansible playbook

This Ansible playbook has the following purposes:
- Install IBM Wazi Deploy on z/OS by using the Python wheel method
- Install IBM Wazi Deploy on a controller for an Ansible usage
- Fetch the IBM Wazi Deploy installable file (`wazideploy.tar`) file to let you install it

## Prerequisites

### On the z/OS host

1. **IBM Open Enterprise SDK for Python**
   - Python version to install: Python 3.11, 3.12, 3.13 or 3.14. See the Wazi Deploy [Installation requirements](https://www.ibm.com/docs/en/SSQ2R2_latest/com.ibm.wazi.deploy.doc/wd_install_prereq.html) for the latest requirements.
   - Default location: `/python/usr/lpp/IBM/cyp/v3r13/pyz`. Adapt the Python version number.
   - Verify installation: `python3 --version`.

2. **Wazi Deploy package**   

   The Wazi Deploy package is the `wazideploy.tar` file.
   - It must be present on the z/OS system.
   - It must be placed in the installation directory (`/usr/lpp/IBM/gdp`) under the `tar` directory.
   - It contains wheel files and dependencies in the installation directory.

3. **SSH access and user permissions**
   - SSH service must be running on z/OS.
   - User must have appropriate permissions to:
     - Create directories in the installation directory
     - Install Python packages
     - Execute Python commands
     - Read and extract tar files
     - Create a Python virtual environment that could be shared with other users if needed

4. **Required z/OS Commands**
   - `tar` - for extracting `.tar` files
   - `mkdir` - for creating directories

### On the control machine (where Ansible runs)

1. **Ansible**
   - Ansible: Ansible core version 2.9 or later. See the Wazi Deploy [Installation requirements](https://www.ibm.com/docs/en/SSQ2R2_latest/com.ibm.wazi.deploy.doc/wd_install_prereq.html) for the latest requirements.
   - Install: `pip install ansible`

2. **Network Connectivity**
   - SSH access to z/OS host (port 22)
   - Network connectivity verified

3. **SSH Configuration**
   - SSH client configured
   - User credentials for z/OS system

## Files

- `wd-installation.yml` - Main Ansible playbook
- `task_fetch.yml` - Task to fetch `wazideploy.tar` from the z/OS
- `hosts.yml` - Inventory file with z/OS host configuration

## Configuration

### Inventory file (hosts.yml)

```
all:
  hosts:
    zos_host:
      ansible_host: <ZOS_IP_ADDRESS>
      ansible_port: 22
      ansible_user: <ZOS_USERNAME>
      ansible_connection: ssh
      ansible_shell_type: sh
      ansible_python_interpreter: "{{ zos_python_installation_path }}/bin/python3"  
```

Set the value of the following variables:
- `<ZOS_IP_ADDRESS>` - IP address of your z/OS system
- `<ZOS_USERNAME>` - Your z/OS username

### Variables

The playbook uses the following variables:

| Variable | Description | Default | Optional |
|----------|-------------|---------|----------|
| `zos_smpe_installation_dir` | SMP/E installation dir | `/usr/lpp/IBM/gdp` | Yes |
| `zos_python_installation_path` | Path to Python installation | `/usr/lpp/IBM/cyp/v3r13/pyz` | Yes |
| `zos_python_venv_path` | Python virtual environment path | `/global/opt/pyenv/gdp` | Yes |
| `ZOAU_HOME` | Path to the ZOAU installation | `/var/usr/lpp/IBM/zoautil/v1.3.6.3` | Yes |

Don't forget to replace the default values to match your installation.

You must pass the following variables when you run the playbook:
- `install_mode`: The installation mode (`zos`, `controller`, or `fetch`):
   - `zos` - Default. Runs on z/OS. Installs the Python path for Wazi Deploy.
   - `controller` - Runs on the distributed system. Installs the Ansible path for Wazi Deploy.
   - `fetch` - Runs on the distributed system. Fetches the Wazi Deploy package from the z/OS system.
- `tar_file_location`: The location where you want to copy the `.tar` file from the z/OS system to the controller. This option is used only if you are in `fetch` mode.   

The `install_mode` and `tar_file_location` variables are required for the playbook to run. They are verified when you launch the playbook.

## Usage

#### 1. Install on zOS the Wazi Deploy version installed via SMP/E
```bash
ansible-playbook wd-installation.yml -i hosts.yml -e "install_mode=zos"
```

#### 2. Install on the Controller the Wazi Deploy version installed via SMP/E
```bash
ansible-playbook wd-installation.yml -i hosts.yml -e "install_mode=controller"
```

#### 3. Only fetch the `wazideploy.tar` file from the zOS and put it in the `/home/XXX/wazi_deploy_folder` folder
```bash
ansible-playbook wd-installation.yml -i hosts.yml -e "install_mode=fetch" -e "tar_file_location=/home/XXX/wazi_deploy_folder/"
```

## Support

For issues related to:
- **Wazi Deploy**: Refer to the [IBM Wazi Deploy documentation](https://www.ibm.com/docs/en/developer-for-zos/latest).
- **Ansible**: Check the [Ansible documentation](https://docs.ansible.com/).
- **z/OS Python**: Read the [IBM Open Enterprise SDK for Python documentation](https://www.ibm.com/docs/en/python-zos/latest).

## Version information

The default values apply to Wazi Deploy 3.0.8. Make sure to adapt the values to match your environment. 

## License

Refer to the license files in the `license/` directory for Wazi Deploy licensing information.
