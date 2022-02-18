@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import groovy.transform.*
import groovy.json.JsonSlurper
import groovy.cli.commons.*

import java.security.KeyManagementException
import java.security.NoSuchAlgorithmException
import java.security.SecureRandom
import java.security.cert.CertificateException
import java.security.cert.X509Certificate

import javax.net.ssl.HttpsURLConnection
import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManager
import javax.net.ssl.X509TrustManager

import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.impl.client.HttpClientBuilder
import org.apache.http.client.methods.HttpPut
import org.apache.http.client.methods.HttpGet
import org.apache.http.conn.ssl.SSLConnectionSocketFactory
import org.apache.http.conn.ssl.SSLContextBuilder
import org.apache.http.conn.ssl.TrustStrategy
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClients
import org.apache.http.Header
import org.apache.http.message.BasicHeader
import org.apache.http.entity.StringEntity
import org.apache.http.util.EntityUtils
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.protocol.HttpClientContext
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.impl.client.BasicCredentialsProvider
import org.apache.http.client.CredentialsProvider
import org.apache.http.auth.AuthScope

// Very basic script to perform UCD application process deployment
enum Status {
	PENDING, EXECUTING, SUCCEEDED, FAULTED
}

@Field DEFAULT_SSL_PROTOCOLS = "TLSv1.2"
@Field int timeout = 60000
@Field requestConfig = RequestConfig.custom()
				.setConnectionRequestTimeout(timeout)
				.setConnectTimeout(timeout)
				.setSocketTimeout(timeout)
				.build()

//@Field clientBuilder = HttpClients.custom()			
@Field clientBuilder = HttpClients.custom().useSystemProperties()

// Can only run in the context of groovyz
def getHttpClient(boolean disableSSLVerify, String sslProtocols) {
	CloseableHttpClient httpClient = null
	//Set up the HttpClient to bypass SSL certicate untrusted issue if needed.
	SSLContextBuilder builder = new SSLContextBuilder()
	if ( disableSSLVerify ) {
		builder.loadTrustMaterial(null, new TrustStrategy()
			{
				@Override
				public boolean isTrusted(X509Certificate[] chain, String authType) throws CertificateException
				{
					return true
				}
			});
	}
	SSLConnectionSocketFactory sslConnectionSocketFactory = 
		new SSLConnectionSocketFactory(builder.build(),
			sslProtocols.split(","), 
			null,
			SSLConnectionSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER )
	clientBuilder.setSSLSocketFactory(sslConnectionSocketFactory)
	httpClient = clientBuilder.build()
	return httpClient
}
	
//* This script request an application process deployment
 run(args)


 def deploy ( String url, String user, String password, String application,
			 String applicationProcess, String environment, String deployVersions, String timeout, boolean disableSSLVerify, boolean verbose, String sslProtocols ) {
	
	println "**  Deploying component versions: $deployVersions"
	println "*** Starting deployment process '$applicationProcess' of application '$application' in environment '$environment'"

	def rc = 0
	def urlString= "$url/cli/applicationProcessRequest/request"
	def httpClient = getHttpClient(disableSSLVerify, sslProtocols)
	def request = new HttpPut(urlString)
	request.addHeader(new BasicHeader("Content-Type", "application/json"))
	
	
	HttpClientContext clientContext = HttpClientContext.create();
	
	CredentialsProvider provider = new BasicCredentialsProvider();
	UsernamePasswordCredentials credentials = new UsernamePasswordCredentials("$user", "$password");
	provider.setCredentials(AuthScope.ANY, credentials);
	clientContext.setCredentialsProvider(provider);
	
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
	
	if ( verbose)
		println "*** Request payload:\n$jsonString"
	
	def entity =  new StringEntity(new String (jsonString, "UTF-8"))
	request.setEntity(entity)
	def response = httpClient.execute(request,clientContext)
	def statusCode = response.getStatusLine().getStatusCode()
	if ( verbose)
		println "*** Status Code: $statusCode"
	def responseString = EntityUtils.toString(response.getEntity())
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
		print "*** Follow Process Request: $requestUrl "
		request = new HttpGet("$url/cli/applicationProcessRequest/requestStatus?request=$requestId")
		retry:
		while ([Status.PENDING, Status.EXECUTING].contains(deployStatus) && after - before <= timeoutMilliseconds && statusCode == 200) {
			after = System.currentTimeMillis()
			response = httpClient.execute(request,clientContext)
			statusCode = response.getStatusLine().getStatusCode()
			responseString = EntityUtils.toString(response.getEntity())
			json = slurper.parseText("$responseString")
			if ( verbose ) {
				println "*** Status Code: $statusCode"
				println "*** Current status: " + json["status"]
				println "*** $json"
			}
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

//Parsing the command line
def run(String[] cliArgs)
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

	if (opts.h)
	{
		cli.usage()
		System.exit(0)
	}
	
	def rc = deploy (opts.u, opts.U, opts.P, opts.a, opts.p, opts.e, opts.d, opts.t ? opts.t : "300000", opts.k, opts.v, opts.s ? opts.s : DEFAULT_SSL_PROTOCOLS)
	
	if  ( rc != 0 ) {
		System.exit(1)
	}
}