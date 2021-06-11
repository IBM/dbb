# IBM UCD application process deployment utility script

This groovy script can be use to deploy an application component version into a specific environment

Example invocation:
```
$DBB_HOME/bin/groovyz ucd-deploy.groovy  -a "GenApp-Deploy" -e "Development" -U XXXX -P XXXX -u https://ucd.server.com:8443 -d "GenAppComponent:latest" -p Deploy -k
```

## Command Line Options Summary
```
$DBB_HOME/bin/groovyz <ussLocation>/ucd-deploy.groovy [options]

required options:
 -h,--help                                      Prints this message
 -u,--url <url>                                 The UCD server URL
 -U,--user <user>                               The UCD user name
 -P,--password <password>                       The UCD password
 -a,--application <application>                 The UCD application name
 -p,--applicationProcess <application process>  The UCD application process name
 -e,--environment <environment>                 The UCD application environment name
 -d,--deployVersions <versions to deploy>       The versions to deploy in the format "Comp1:latest\nComp2:latest"
 -t,--deployTimeout                             The deployment timeout in seconds (default 300s)
 -s,--sslProtocols                              The SSL protocols to handle in the format "TLSv1.2,TLSv1.3". Default is TLSv1.2
 -k,--disableSSLVerify                          Disable SSL verification
 -v,--verbose                                   Flag to turn on script trace
 ```