@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import groovy.transform.*
import groovy.json.JsonSlurper

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
@Field int timeout = 60000
@Field requestConfig = RequestConfig.custom()
				.setConnectionRequestTimeout(timeout)
				.setConnectTimeout(timeout)
				.setSocketTimeout(timeout)
				.build()
@Field clientBuilder = HttpClients.custom()

// Can only run in the context of groovyz
def getHttpClient(boolean disableSSLVerify) {
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
		new SSLConnectionSocketFactory(builder.build(), null, null,
			SSLConnectionSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER )
	clientBuilder.setSSLSocketFactory(sslConnectionSocketFactory)
	httpClient = clientBuilder.build()
	return httpClient
}
	
//* This script request an application process deployment
 run(args)


 def deploy ( String url, String user, String password, String application,
			 String applicationProcess, String environment, String deployVersions, boolean disableSSLVerify, boolean verbose) {
	
	println "**  Deploying component versions: $deployVersions"
	println "*** Starting deployment process '$applicationProcess' of application '$application' in environment '$environment'"

	def rc = 0
	def urlString= "$url/cli/applicationProcessRequest/request"
	def httpClient = getHttpClient(disableSSLVerify)
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
	println "*** Status Code: $statusCode"
	def responseString = EntityUtils.toString(response.getEntity())
	if ( statusCode != 200 ) {
		rc = 1
		println("*** Deployment failed:\n")
		println "$responseString"
	} else {
		Thread.sleep( 1000 )
		JsonSlurper slurper = new groovy.json.JsonSlurper()
		def json = slurper.parseText("$responseString")
		def requestId = json["requestId"]
		def requestUrl = "$url/#applicationProcessRequest/$requestId" 
		println "*** Follow Process Request: $requestUrl"
		request = new HttpGet("$url/cli/applicationProcessRequest/$requestId")
		response = httpClient.execute(request,clientContext)
		statusCode = response.getStatusLine().getStatusCode()
		responseString = EntityUtils.toString(response.getEntity())
		json = slurper.parseText("$responseString")
		while (statusCode == 200 
			&& ( json["state"] == "EXECUTING" || json["state"] == "INITIALIZED" ) ) {
			print "."
			response = httpClient.execute(request,clientContext)
			statusCode = response.getStatusLine().getStatusCode()
			responseString = EntityUtils.toString(response.getEntity())
			json = slurper.parseText("$responseString")
			Thread.sleep( 1000 )
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
	
	def rc = deploy (opts.u, opts.U, opts.P, opts.a, opts.p, opts.e, opts.d, opts.k, opts.v)
	
	if  ( rc != 0 ) {
		System.exit(1)
	}
}