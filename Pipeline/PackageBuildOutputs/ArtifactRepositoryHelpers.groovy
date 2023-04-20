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


/** Very basic script to upload/download from an artifact repository server
 *
 * Version 1 - 2022
 * 
 * This script requires JAVA 11 because it uses java.net.http.* APIs to create
 * the HTTPClient and HTTPRequest, replacing the previous ArtifactoryHelper
 *
 * Version 2 - 2023-04
 * 
 * The script has been enhanced to support retries when uploading to an Artifact Repository
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

def upload(String url, String fileName, String user, String password, boolean verbose) throws IOException {
    println( "** ArtifactRepositoryHelper started for upload of $fileName to $url..." );
    
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
    .header("Content-Type", "binary/octet-stream")
    .PUT(BodyPublishers.ofFile(Paths.get(fileName)))
    .build();

	println( "** Uploading $fileName to $url..." );
	
	HttpResponse.BodyHandler<String> handler = HttpResponse.BodyHandlers.ofString();
    // submit request
	CompletableFuture<HttpResponse<String>> response = httpClient.sendAsync(request, handler).thenComposeAsync(r -> tryResend(httpClient, request, handler, 1, r));
	HttpResponse finalResponse = response.get()
    
    if ( verbose ) println( "** Response: " + finalResponse );
    
    def rc = evaluateHttpResponse(finalResponse, "upload", verbose)
    if (rc == 0 ) {
        println("** Upload completed");
    }
    else {
        println("** Upload failed");
    }
}

def download(String url, String fileName, String user, String password, boolean verbose) throws IOException  {
    println( "** ArtifactRepositoryHelper started for download of $url to $fileName..." );
   
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
    println( "** Downloading $url to $fileName..." );
	HttpResponse.BodyHandler<String> handler = HttpResponse.BodyHandlers.ofString();
	CompletableFuture<HttpResponse<String>> response = httpClient.sendAsync(request, handler).thenComposeAsync(r -> tryResend(httpClient, request, handler, 1, r));

	HttpResponse finalResponse = response.get()
	
    // evalulate response
    rc = evaluateHttpResponse(finalResponse, "download", verbose)
    
    if (rc == 0) {
        // write file to output 
        def responseBody = finalResponse.body()
        println("** Writing to file to $fileName...")
        FileOutputStream fos = new FileOutputStream(fileName);
        fos.write(responseBody.readAllBytes());
        fos.close();
    } else {
        println("*! Download failed");
    }
}

def evaluateHttpResponse (HttpResponse response, String action, boolean verbose) {
    int rc = 0
    def statusCode = response.statusCode()
    if ( verbose) println "*** HTTP-Status Code: $statusCode"
    def responseString = response.body()
    if ( (statusCode != 201) && (statusCode != 200)  ) {
        rc = 1
        println("** Artifactory $action failed with statusCode : $statusCode ")
        println( "** Response: " + response );
        throw new RuntimeException("Exception : Artifactory $action failed: "
             + statusCode);
    }
    
    return rc
}


//Parsing the command line
def run(String[] cliArgs) {

    def cli = new CliBuilder(usage: "ArtifactRepositoryHelpers.groovy [options]", header: '', stopAtNonOption: false)
    cli.h(longOpt:'help', 'Prints this message')
    cli.u(longOpt:'url', args:1, required:true, 'Artifactory file uri location')
    cli.fU(longOpt:'fileToUpload', args:1, 'The full path of the file to upload')
    cli.fD(longOpt:'fileToDownload', args:1, 'The full path of the file to download')
    cli.U(longOpt:'user', args:1, required:true, 'Artifactory user id')
    cli.P(longOpt:'password', args:1, required:true, 'Artifactory password')
    cli.v(longOpt:'verbose', 'Flag to turn on script trace')
    def opts = cli.parse(cliArgs)

    // if opt parse fail exit.
    if (! opts) {
        System.exit(1)
    }

    if (opts.h) {
        cli.usage()
        System.exit(0)
    }
    
    if (opts.fU) {
        upload(opts.u, opts.fU, opts.U, opts.P, opts.v)
    } else {
        download(opts.u, opts.fD, opts.U, opts.P, opts.v)
    }
}