# WebAppCleanUp Utility

The WebAppCleanUp utility is used to delete DBB web application collections and build groups that are no longer needed. A good example of this is when using the [zAppBuild sample application](https://github.com/IBM/dbb-zappbuild) for [topic branch](https://git-scm.com/book/en/v2/Git-Branching-Branching-Workflows#_topic_branch) builds of z/OS applications. During the build a build group and two collections will be created and continued to be updated as more builds are run.  The naming convention for these artifacts created by zAppBuild uses the name of the application and topic branch being developed:
```
Build Group:
Application-TopicBranch

Collections:
Application-TopicBranch
Application-TopicBranch-outputs
```

Once development is completed and the topic branch is merged back into the main development branch and deleted, the WebAppCleanUp utility script can be used to easily delete the two collections and build group that was created for the topic branch.  Example:
```
$DBB_HOME/bin/groovyz WebAppCleanUp.groovy --groups Application-TopicBranch --collections Application-TopicBranch,Application-TopicBranch-outputs
```
output:
```
Creating repository client for https://localhost:9443/dbb/
** Deleting build groups: [Application-TopicBranch]
*** Deleting build group 'Application-TopicBranch' -> Status = HTTP/1.1 200 OK
** Deleting collections: [Application-TopicBranch, Application-TopicBranch-outputs]
*** Deleting collection 'Application-TopicBranch' -> Status = HTTP/1.1 200 OK
*** Deleting collection 'Application-TopicBranch-outputs' -> Status = HTTP/1.1 200 OK
** Build finished
```

### Bulk Deletes
When a large number of build groups and/or collections are needed to be deleted, the WebAppCleanUp.groovy script also supports passing in text files which contain a list of build groups or collections to delete. Example:
```
$DBB_HOME/bin/groovyz WebAppCleanUp.groovy --groupsFile groups.txt --collectionsFile collections.txt
```
The format of the deletion list files are simple text with one build group or collection name per line.  Example of `collectons.txt`:
```
MortgageApplication-MQSupport
MortgageApplication-MQSupport-outputs
MortgageApplication-Bug1220
MortgageApplication-Bug1220-outputs
```

### Web Application Authentication Properties
A property file can be specified through the --prop parameter. This property file can stored DBB Web Application authentication properties. A sample property file `user.properties` is supplied along this script. 
 However its use is optional as the user can provide the required authentication values as script arguments.  Example:
```
$DBB_HOME/bin/groovyz WebAppCleanUp.groovy --groups Application-FeatureBranch --url https://localhost:9443/dbb --id ADMIN --pw ADMIN
``` 

### WebAppCleanUp.groovy Command Line Options
```
usage: WebAppCleanUp.groovy [options]
options:
 -c,--collections <arg>       Comma separated list of Collections to
                              delete
 -C,--collectionsFile <arg>   Absolute or relative path (from script
                              directory) of file containing collections to
                              delete
 -g,--groups <arg>            Comma separated list of Build Groups to
                              delete
 -G,--groupsFile <arg>        Absolute or relative path (from script
                              directory) of file containing build groups
                              to delete
 -h,--help                    Prints this message
 -i,--id <arg>                DBB WebApp ID
 -p,--pw <arg>                DBB WebApp Password
 -P,--pwFile <arg>            Absolute or relative (from this script) path
                              to file containing DBB password   
 -prop,--propertyFile <arg>   Absolute or relative (from this script) path
                              to property file that contains DBB WebApp
                              information (Optional)                                                 
 -u,--url <arg>               DBB WebApp URL
```
