# IBM UCD application process deployment utility script

This groovy script can be used to request an UCD application deployment of a previously created UCD component version into a specific environment.

**Please note:** This script leverages `java.net.http.HttpClient` which got added in JAVA 11. Therefore this script requires JAVA 11 and DBB 2.0.

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

## Console output

```
/usr/lpp/dbb/v2r0/bin/groovyz ucd-deploy.groovy -a "CatalogManager" -e "Integration-Test" -U XXXX -P XXXX -u https://ucd.server.com:8443 -d "CatalogManager:rel-2.10.1-7076" -p appDeployment -k
** Request UCD Deployment start at 20221130.112746.027
** Properties at startup:
   application -> CatalogManager
   environment -> Integration-Test
   user -> admin
   password -> xxxxxx
   url -> https://ucd.server.com:8443
   deployVersions -> CatalogManager:rel-2.10.1-7076
   applicationProcess -> appDeployment
   disableSSLVerify -> true
**  Deploying component versions: CatalogManager:rel-2.10.1-7076
*** Starting deployment process 'appDeployment' of application 'CatalogManager' in environment 'Integration-Test'
*** SSL Verification disabled
*** Follow Process Request: https://ucd.server.com:8443/#applicationProcessRequest/184c812f-605f-5040-ad31-d3a31f87bb3c
Executing ......
*** The deployment result is SUCCEEDED. See the UrbanCode Deploy deployment logs for details.
** Build finished
```