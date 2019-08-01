
# SCLM Migration Sample
This sample provides scripts to migrate source members from IBM Software Configuration and Library Manager (SCLM) to a local Git repository. Additionally an update is planned for January 2019 to also include scripts to generate Groovy/DBB build script stubs (may require configuration) from SCLM build definitions.

## Overview
* The sample is a combination of Unix shell scripts, Apache Groovy scripts and REXX scripts
* Uses the existing [DBB migration tool](https://www.ibm.com/support/knowledgecenter/SS6T76_1.0.2/migration.html)
* Users can choose to how many versions to migrate thus preserving change history 

## Assumptions
Please review the [SCLM Assumptions documentation](https://github.com/IBM/dbb/blob/master/Migration/sclm/sclmAssumptions.md)  before running the migration process.

## Prerequisites
* DBB Toolkit
    * DBB_HOME environment variable must be set
* Rocket’s Git and supporting open source tools
* IBM Java v8 64bit
    * JAVA_HOME environment variable must be set
* ISPF Legacy Gateway

## Folder Content
* bin - Contains the shell scripts that drive the migration process
* conf - Contains the sclmmig.config file that must be edited before the migration process is started
* groovy - Contains Groovy/DBB scripts that are invoked by the shell scripts
* rexx - Contains REXX scripts used to extract SCLM metadata
* template - Contains template for the generated Groovy scripts

## Configuration
* Fill in information related to the SCLM project in conf/sclmmig.config file.

# Migration Process
The migration process is comprised of three phases each of which contain multiple steps.  
* SCLM Extraction
* Source Code Migration
* Build Script Generation

Step by step instructions are located in the [Migration Process](https://github.com/IBM/dbb/blob/master/Migration/sclm/migrationSteps.md) documentation.

