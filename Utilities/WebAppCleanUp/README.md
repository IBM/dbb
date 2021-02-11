# WebAppCleanUp Utility

The WebAppCleanUp utility is used to delete DBB web application collections and build groups that are no longer needed. A good example of this is when using the [zAppBuild sample application](https://github.com/IBM/dbb-zappbuild) for topic branch builds of z/OS applications. During the build a build group and two collections will be created and continued to be updated as more builds are run.  The naming convention uses the name of the application and topic branch being developed:
```
Build Group:
Application-TopicBranch

Collections:
Application-TopicBranch
Application-TopicBranch-outputs
```

One development in completed and the topic branch is merge back into the main development branch, the WebAppCleanUp utility script can be used to easily delete the two collections and build group that was created for the topic branch.  Example:
```
$DBBHOME/bin/groovyz WebAppCleanUp.groovy --groups Application-FeatureBranch --collections Application-FeatureBranch,Application-FeatureBranch-outputs
```
