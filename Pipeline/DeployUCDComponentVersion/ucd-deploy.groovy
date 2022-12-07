@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import groovy.transform.*
import groovy.json.JsonSlurper
import groovy.cli.commons.*

import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManager
import javax.net.ssl.X509TrustManager

import java.security.cert.CertificateException
import java.security.cert.X509Certificate

import java.net.http.*
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers

// Very basic script to perform UCD application process deployment
enum Status {
	PENDING, EXECUTING, SUCCEEDED, FAULTED
}

@Field DEFAULT_SSL_PROTOCOLS = "TLSv1.2"
@Field int timeout = 60000

/** This script request an application process deployment
 * 
 * Version 1 - 2022
 * 
 *  Uplift for DBB 2.0
 * 
 *  This script requires JAVA 11 because it uses java.net.http.* APIs to create
 *  the HTTPClient and HTTPRequest 
 *
 */

// establish environment
def opts = parseInput(args)

// print
def startTime = new Date().format("yyyyMMdd.hhmmss.mmm")
println("** Request UCD Deployment start at $startTime")
println("** Properties at startup:")
opts.getOptions().each{option ->
	if ( option.getLongOpt() == "password" )
		println "   ${option.getLongOpt()} -> xxxxxx "
	else
		println "   ${option.getLongOpt()} -> ${(option.getValue() ? option.getValue() : 'true' )} "
}

def rc = requestDeployment(opts.url, opts.user, opts.password,
	opts.application, opts.applicationProcess, opts.environment,
	opts.deployVersions, opts.timeout ? opts.timeout : "300000",
	opts.disableSSLVerify.toBoolean(), opts.verbose.toBoolean(),
	opts.sslProtocols ? opts.sslProtocols : DEFAULT_SSL_PROTOCOLS)

if  ( rc != 0 ) {
	println("*** Deployment terminated with an error. ")
	System.exit(rc)
}

// method to submit the request and poll for result

def requestDeployment( String url, String user, String password, String application,
String applicationProcess, String environment, String deployVersions, String timeout, boolean disableSSLVerify, boolean verbose, String sslProtocols ) {

	println "**  Deploying component versions: $deployVersions"
	println "*** Starting deployment process '$applicationProcess' of application '$application' in environment '$environment'"

	def rc = 0
	def urlString= "$url/cli/applicationProcessRequest/request"

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

	if ( disableSSLVerify ) {
		SSLContext sc = SSLContext.getInstance(DEFAULT_SSL_PROTOCOLS);
		sc.init(null, trustAllCertsTrustManager(), null);
		httpClientBuilder.sslContext(sc)
	}

	HttpClient httpClient = httpClientBuilder.build();

	// build request including body
	def jsonString = """
{
  "application": "$application",
  "applicationProcess": "$applicationProcess",
  "environment": "$environment",
  "versions": [
"""

	def deployVersionsList = deployVersions.split("\\n")
	deployVersionsList.each { deployVersion ->
		def items = deployVersion.split(":")
		def component = items[0]
		def version = items[1]
		jsonString += """
	{
	  "version": "$version",
	  "component": "$component"
	},
"""
	}

	jsonString += "    ]\n}"

	if (verbose) println "*** Request payload:\n$jsonString"


	// build http request
	HttpRequest request = HttpRequest.newBuilder()
	.uri(URI.create("$urlString"))
	.header("Content-Type", "application/json")
	.POST(BodyPublishers.ofString(jsonString))
	.build();


	HttpResponse response = httpClient.send(request, BodyHandlers.ofString())

	def statusCode = response.statusCode()
	if ( verbose) println "*** HTTP-Status Code: $statusCode"
	def responseString = response.body()
	if ( statusCode != 200 ) {
		rc = 1
		println("*** Deployment failed:\n")
		println "$responseString"
	} else {
		def Status deployStatus = Status.PENDING
		def before = System.currentTimeMillis()
		def after = before
		def timeoutMilliseconds = Long.parseLong(timeout)
		def retryPeriodMilliseconds = 5000
		def JsonSlurper slurper = new groovy.json.JsonSlurper()
		def json = slurper.parseText("$responseString")
		def requestId = json["requestId"]
		def requestUrl = "$url/#applicationProcessRequest/$requestId"
		println "*** Follow Process Request: $requestUrl "
		print "Executing "
	
		// assemble a new request to obtain the status of the request:
		request = HttpRequest.newBuilder()
				.uri(URI.create("$url/cli/applicationProcessRequest/requestStatus?request=$requestId"))
				.header("Content-Type", "application/json")
				.GET()
				.build();
		
		retry:
			while ([Status.PENDING, Status.EXECUTING].contains(deployStatus) && after - before <= timeoutMilliseconds && statusCode == 200) {
				after = System.currentTimeMillis()
				response = httpClient.send(request, BodyHandlers.ofString())
				statusCode = response.statusCode()
				if ( verbose ) println "*** Status Code: $statusCode"
				responseString = response.body()
				if ( verbose ) println "*** Response String: $responseString"
				json = slurper.parseText("$responseString")
				if ( verbose ) 	println "*** Current status: " + json["status"]
				if ( verbose ) 	println "*** $json"
				
				switch (json["status"]) {
					case "PENDING":
						deployStatus = Status.PENDING
						break
					case "EXECUTING":
						deployStatus = Status.EXECUTING
						break
					case "CLOSED":
						switch (json["result"]) {
							case "SUCCEEDED":
								deployStatus = Status.SUCCEEDED
								break retry
							case "FAULTED":
								deployStatus = Status.FAULTED
								rc=1
								break retry
							default:
								rc=1
								break retry
						}
					default:
						rc=1
						break retry
				}
				print "."
				sleep(retryPeriodMilliseconds)
			}
			println "\n*** The deployment result is " + json["result"] + ". See the UrbanCode Deploy deployment logs for details."
			if ( statusCode != 200 ||  json["result"] != "SUCCEEDED" ) {
				rc = 1
				println("*** Deployment failed:\n")
				println "$responseString"
			}
		}
	return rc
}

// parsing the command line and return the options
def parseInput(String[] cliArgs)
{
	def cli = new CliBuilder(usage: "ucd-deploy.groovy [options]", header: '', stopAtNonOption: false)
	cli.h(longOpt:'help', 'Prints this message')
	cli.u(longOpt:'url', args:1, required:true,'The UCD server URL')
	cli.U(longOpt:'user', args:1, required:true,'The UCD user name')
	cli.P(longOpt:'password', args:1, required:true,'The UCD password')
	cli.a(longOpt:'application', args:1, required:true,'The UCD application name')
	cli.p(longOpt:'applicationProcess', args:1, required:true,'The UCD application process name')
	cli.e(longOpt:'environment', args:1, required:true, 'The UCD application environment name')
	cli.d(longOpt:'deployVersions', args:1, required:true, 'The versions to deploy in the format "Comp1:latest\\nComp2:latest"')
	cli.t(longOpt:'deployTimeout', args:1, 'The deployment timeout in seconds (default 300s)')
	cli.s(longOpt:'sslProtocols', args:1, required:false, 'The SSL protocols to handle in the format "TLSv1.2,TLSv1.3". Default is TLSv1.2')
	cli.k(longOpt:'disableSSLVerify', 'Disable SSL verification')
	cli.v(longOpt:'verbose', 'Flag to turn on script trace')

	def opts = cli.parse(cliArgs)

	// if opt parse fail exit.
	if (! opts) {
		System.exit(1)
	}

	// print help
	if (opts.h)
	{
		cli.usage()
		System.exit(0)
	}

	return opts
}

// returns a TrustManager which skips SSL verification
def trustAllCertsTrustManager() {
	println "*** SSL Verification disabled"
	TrustManager[] trustAllCerts = new TrustManager[] { new X509TrustManager() {
			@Override
			public java.security.cert.X509Certificate[] getAcceptedIssuers() {
				return null;
			}

			@Override
			public void checkClientTrusted(X509Certificate[] certs,
			String authType) {
			}

			@Override
			public void checkServerTrusted(X509Certificate[] certs,
			String authType) {
			}
		} };

	return trustAllCerts
}