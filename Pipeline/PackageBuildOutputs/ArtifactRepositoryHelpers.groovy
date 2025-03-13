@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import groovy.transform.*
import javax.net.ssl.SSLContext
import groovy.cli.commons.*

import java.net.http.*
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.HttpResponse.BodyHandler
import java.util.concurrent.CompletableFuture

import java.nio.file.Paths
import java.nio.file.Path


/** Very basic script to upload/download from an artifact repository server
 *
 * Version 1 - 2022
 * 
 * This script requires JAVA 11 because it uses java.net.http.* APIs to create
 * the HTTPClient and HTTPRequest, replacing the previous ArtifactoryHelper
 *
 * Version 2 - 2023-04
 * 
 * Implemented a retry mechanism for upload and download (with Connection Keep-alive for upload)
 *
 * Version 3 - 2024-05
 * 
 * Supporting different HTTP client configurations
 *
 */

@Field int MAX_RESEND = 10;

def <T> CompletableFuture<HttpResponse<T>>
		tryResend(HttpClient client, HttpRequest request, BodyHandler<T> handler,
				 int count, HttpResponse<T> resp) {
	if (resp.statusCode() == 200 || count >= MAX_RESEND) {
		return CompletableFuture.completedFuture(resp);
	} else {
		return client.sendAsync(request, handler)
				.thenComposeAsync(r -> tryResend(client, request, handler, count+1, r));
	}
}

run(args)

// public methods
def upload(String url, String fileName, String user, String password, boolean verbose, String httpClientVersion) throws IOException {
	System.setProperty("jdk.httpclient.allowRestrictedHeaders", "Connection")
	Path testing = Paths.get(fileName)
	println "asdadasdasdasd ----- ${testing.toString()}"
    println( "** ArtifactRepositoryHelper started for upload of $fileName to $url" );
    
    // create http client
    HttpClient.Builder httpClientBuilder = HttpClient.newBuilder()
    .authenticator(new Authenticator() {
        @Override
        protected PasswordAuthentication getPasswordAuthentication() {
            return new PasswordAuthentication(
            "$user",
            "$password".toCharArray());
        }
    });

//  if ( disableSSLVerify ) {
//      SSLContext sc = SSLContext.getInstance(DEFAULT_SSL_PROTOCOLS);
//      sc.init(null, trustAllCertsTrustManager(), null);
//      httpClientBuilder.sslContext(sc)
//  }

    HttpClient httpClient = httpClientBuilder.build();
    
    // build http request
    HttpRequest.Builder httpRequestBuilder = HttpRequest.newBuilder()
    .uri(URI.create("$url"))
    .header("Content-Type", "binary/octet-stream")
	.header("Connection","Keep-Alive")
    .PUT(BodyPublishers.ofFile(Paths.get(fileName)));

    // set http client version if set
    if (httpClientVersion) {
        def httpVer = HttpClient.Version.valueOf(httpClientVersion)
        if (httpVer) {
           httpRequestBuilder.version(httpVer)
        } else {
	        println("*! $httpClientVersion is invalid. Using default HTTP Client protocol version.");
        }
    }
	
	HttpRequest request =  httpRequestBuilder.build()

	println("** Uploading $fileName to $url...");
	
	HttpResponse.BodyHandler<String> handler = HttpResponse.BodyHandlers.ofString();
    // submit request
	CompletableFuture<HttpResponse<String>> response = httpClient.sendAsync(request, handler).thenComposeAsync(r -> tryResend(httpClient, request, handler, 1, r));
	HttpResponse finalResponse = response.get()
    
    if (verbose)
		println("** Response: " + finalResponse);
    
    def rc = evaluateHttpResponse(finalResponse, "upload", verbose)
    
    if (rc == 0 ) {
        println("** Upload completed.");
    }
    else {
        println("*! Upload failed.");
    }
}

def download(String url, String fileName, String user, String password, boolean verbose) throws IOException  {
    println("** ArtifactRepositoryHelper started for download of $url to $fileName.");

    // create http client
    HttpClient.Builder httpClientBuilder = HttpClient.newBuilder()
    .authenticator(new Authenticator() {
        @Override
        protected PasswordAuthentication getPasswordAuthentication() {
            return new PasswordAuthentication(
            "$user",
            "$password".toCharArray());
        }
    });

//  if ( disableSSLVerify ) {
//      SSLContext sc = SSLContext.getInstance(DEFAULT_SSL_PROTOCOLS);
//      sc.init(null, trustAllCertsTrustManager(), null);
//      httpClientBuilder.sslContext(sc)
//  }

    HttpClient httpClient = httpClientBuilder.build();
    
    // build http request
    HttpRequest request = HttpRequest.newBuilder()
    .uri(URI.create("$url"))
    .GET()
    .build();
    
    // submit request
    println("** Downloading $url to $fileName...");
	HttpResponse.BodyHandler<InputStream> handler = HttpResponse.BodyHandlers.ofInputStream();
	CompletableFuture<HttpResponse<String>> response = httpClient.sendAsync(request, handler).thenComposeAsync(r -> tryResend(httpClient, request, handler, 1, r));

	HttpResponse finalResponse = response.get()
	
    // evalulate response
    rc = evaluateHttpResponse(finalResponse, "download", verbose)
    
    if (rc == 0) {
        // write file to output 
        def responseBody = finalResponse.body()
        println("** Writing to file to $fileName.")
        FileOutputStream fos = new FileOutputStream(fileName);
        fos.write(responseBody.readAllBytes());
        fos.close();
    } else {
        println("*! Download failed.");
    }
}

// Method directly accessed by PackageBuildOutputs and Common Backend script functionality
def computeAbsoluteRepositoryUrl(Properties props) {
	def String remotePath = (props.versionName) ? (props.versionName + "/" + props.tarFileName) : (props.tarFileLabel + "/" + props.tarFileName)
	def url = new URI(props.get('artifactRepository.url') + "/" + props.get('artifactRepository.repo') + "/" + (props.get('artifactRepository.directory') ? "${props.get('artifactRepository.directory')}/" : "") + remotePath).normalize().toString() // Normalized URL
	return url
}

// private methods
def evaluateHttpResponse (HttpResponse response, String action, boolean verbose) {
    int rc = 0
    def statusCode = response.statusCode()
    if (verbose) println "*** HTTP-Status Code: $statusCode"
    def responseString = response.body()
    if ((statusCode != 201) && (statusCode != 200)) {
        rc = 1
        println("** Artifactory $action failed with statusCode : $statusCode")
        println("** Response: " + response);
        throw new RuntimeException("Exception : Artifactory $action failed:" + statusCode);
    }    
    return rc
}

//Parsing the command line
def run(String[] cliArgs) {
    def cli = new CliBuilder(usage: "ArtifactRepositoryHelpers.groovy [options]", header: '', stopAtNonOption: false)
	def Properties props = new Properties()
    cli.h(longOpt:'help', 'Prints this message')
    cli.u(longOpt:'url', args:1,'Absolute artifact repository url location to store package')
    cli.fU(longOpt:'fileToUpload', args:1, 'The full path of the file to upload')
    cli.fD(longOpt:'fileToDownload', args:1, 'The full path of the file to download')
    cli.U(longOpt:'user', args:1,'Artifact repository user id or token')
    cli.P(longOpt:'password', args:1, 'Artifact repository password')
	cli.ht(longOpt:'httpClientVersion', args:1, 'HTTP Client protocol version')
    cli.v(longOpt:'verbose', 'Flag to turn on script trace')
	
	// recompute options
	// Compute Flag to recompute url

    cli.c(longOpt:'computePackageUrl', 'Action Flag to identify to recompute the uri of a given package')
	cli.t(longOpt:'tarFileName', args:1, argName:'filename', 'Name of the package tar file. (Optional unless using --buildReportOrder or --buildReportOrderFile)')
	cli.aRU(longOpt:'artifactRepositoryUrl', args:1, 'Artifact repository url')
	cli.aRN(longOpt:'artifactRepositoryName', args:1, 'Artifact repository name')
	cli.aRD(longOpt:'artifactRepositoryDirectory', args:1, 'Artifact repository directory')
	cli.aVN(longOpt:'versionName', args:1, argName:'versionName', 'Name of the version/package folder on the Artifact repository server.')
	
    def opts = cli.parse(cliArgs)

    // if opt parsing fails, exit
    if (opts == null || !opts) {
        System.exit(1)
    }
	
	if (opts.c) props.computePackageUrl = true
	if (opts.t) props.tarFileName = opts.t
	if (opts.aRU) props.put('artifactRepository.url', opts.aRU)
	if (opts.aRN) props.put('artifactRepository.repo', opts.aRN)
	if (opts.aRD) props.put('artifactRepository.directory', opts.aRD)
	if (opts.aVN) props.versionName = opts.aVN
			
			

    if (opts.h) {
        cli.usage()
        System.exit(0)
    }
    
    if ( opts.fU) {
		// assert required CLI options for upload
		assert opts.u : "Missing option: Absolute artifact repository url location to store package"
		assert opts.U : "Missing option: Artifact repository user id or token"
		assert opts.P : "Missing option: Artifactory password"
        upload(opts.u, opts.fU, opts.U, opts.P, opts.v)
    } else if (opts.fD) {
		// assert required CLI options for download
		assert opts.u : "Missing option: Absolute artifact repository url location to store package"
		assert opts.U : "Missing option: Artifact repository user id or token"
		assert opts.P : "Missing option: Artifactory password"
        download(opts.u, opts.fD, opts.U, opts.P, opts.v)
    } else if (props.computePackageUrl){
		
		// invoke processing
		if (props.computePackageUrl && props.computePackageUrl.toBoolean()) {
			// check requires cli arguments for this operation
			assert props.tarFileName : "Missing option tarFileName (--tarFileName)"
			assert props.versionName : "Missing option versionName (--versionName)"
			assert props.get('artifactRepository.url') : "Missing option artifactRepository.url (--artifactRepositoryUrl)"
			assert props.get('artifactRepository.repo'): "Missing option artifactRepository.repo (--artifactRepositoryName)"
			assert props.get('artifactRepository.directory'): "Missing option artifactRepository.directory (--artifactRepositoryDirectory)"
			
			// load script	
			def scriptDir = new File(getClass().protectionDomain.codeSource.location.path).parent
			packageUrl = computeAbsoluteRepositoryUrl(props)
			// the println is used in a script by the CBS to grep the packageUrl
			println "packageUrl=$packageUrl"
		} else 		
		println("** No action has been specified for the ArtifactoryHelpers (available action triggers 'fileToUpload' or 'fileToDownload' or 'computePackageUrl') ");
	}
}

