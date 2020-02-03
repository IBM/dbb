# Language Build Scripts
zAppBuild comes with a number of language specific build scripts.  These script are designed to be invoked by `build.groovy` and should not be invoked directly as they require the initialized environment provided by `build.groovy`.

## Included Language Scripts
* Assembler.groovy
* BMS.groovy
* Cobol.groovy
* LinkEdit.groovy (for building link cards)
* PLI.groovy
* DBDgen.groovy
* PSBgen.groovy
* MFS.groovy

All language scripts both compile and optionally link-edit programs. The language build scripts are intended to be useful out of the box but depending on the complexity of your applications' build requirements, may require modifications to meet your development team's needs.  By following the examples used in the existing language build scripts of keeping all application specific references out of the build scripts and instead using configuration properties with strong default values, the zAppBuild sample can continue to be a generic build solution for all of your specific applications.

## Script Mappings
Source files are mapped to language scripts via ***script mapping file properties***. Though script mappings can be defined at any level, zAppBuild relegates script mapping declarations to the application configuration folder (see [application/application-conf/file.properties](../application/application-conf/file.properties) allowing the application owner to determine what is best for the application. 
