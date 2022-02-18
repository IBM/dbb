@groovy.transform.BaseScript com.ibm.dbb.groovy.ScriptLoader baseScript
import groovy.transform.*
import groovy.cli.commons.*

import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.HttpClient;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.entity.FileEntity;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.protocol.HTTP;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.protocol.HttpContext;
import org.apache.http.impl.client.DefaultHttpRequestRetryHandler;

// Very basic script to upload/download from artifactory (replacement of curl)
@Field int retryCount = 50
@Field int conPoolSize = 10
@Field int timeout = 300 // 5 Minutes in seconds
@Field int timeoutMilliSeconds = timeout * 1000;

class CustomRetryHandler extends DefaultHttpRequestRetryHandler {
	boolean verbose = false
	CustomRetryHandler(int connectionRetries, boolean verbose) {
		super(connectionRetries, true);
		this.verbose = verbose
	}

	@Override
	public boolean retryRequest(IOException exception, int totalCount, HttpContext context) {
		HttpClientContext clientContext = HttpClientContext.adapt(context);
		if ( verbose) println("*? WARNING: Error occurred for request " + clientContext.getRequest().getRequestLine().toString() + ": " + exception.getMessage() + ".");
		if (totalCount > retryCount) {
			println("*? Error occurred for request " + clientContext.getRequest().getRequestLine().toString() + ": " + exception.getMessage() + ".");
			return false;
		}
		Thread.sleep(1000);
		boolean shouldRetry = super.retryRequest(exception, totalCount, context);
		if (shouldRetry) {
			if ( verbose) println("*? WARNING: Attempting retry #" + totalCount);
			return true;
		}
		return false;
	}
}

run(args)

def upload(String url, String fileName, String user, String password, boolean verbose) throws IOException {

	RequestConfig requestConfig = RequestConfig
		.custom()
		.setSocketTimeout(timeoutMilliSeconds)
		.setConnectTimeout(timeoutMilliSeconds)
		.setCircularRedirectsAllowed(true)
		.setExpectContinueEnabled(true)
		.build();
	
	HttpPut httpPut = new HttpPut(url);
	httpPut.setConfig(requestConfig);
	httpPut.addHeader(HTTP.EXPECT_DIRECTIVE, HTTP.EXPECT_CONTINUE);
	httpPut.addHeader(HTTP.CONN_DIRECTIVE,HTTP.CONN_KEEP_ALIVE);
	
	if ( verbose ) println( "** Headers: " + httpPut.getAllHeaders());
	if ( verbose ) println( "** Request: " + httpPut );
	
	FileEntity fileEntity = new FileEntity(new File(fileName));
	fileEntity.setContentType("binary/octet-stream");
	httpPut.setEntity(fileEntity);

	HttpClientContext clientContext = HttpClientContext.create();

	CredentialsProvider provider = new BasicCredentialsProvider();
	UsernamePasswordCredentials credentials = new UsernamePasswordCredentials(user, password);
	provider.setCredentials(AuthScope.ANY, credentials);
	clientContext.setCredentialsProvider(provider);

	PoolingHttpClientConnectionManager connectionManager = new PoolingHttpClientConnectionManager();
	connectionManager.setMaxTotal(conPoolSize);
	connectionManager.setDefaultMaxPerRoute(conPoolSize);

	HttpClientBuilder builder = HttpClientBuilder.create().setConnectionManager(connectionManager).setDefaultRequestConfig(requestConfig).useSystemProperties()
	builder.setRetryHandler(new CustomRetryHandler(retryCount, verbose));


	CloseableHttpClient httpClient = builder.build()
	
	HttpResponse response = httpClient.execute(httpPut, clientContext);
	
	if ( verbose ) println( "** Response: " + response );
		
	int statusCode = response.getStatusLine().getStatusCode();

	if ((statusCode != HttpStatus.SC_CREATED) && (statusCode != HttpStatus.SC_OK)) {
		if (response.getEntity() != null) {
			InputStream source = response.getEntity().getContent();
			byte[] buffer = new byte[8192];
			int read;
			while ((read = source.read(buffer)) != -1) {
				System.err.write(buffer, 0, read);
			}
		}
		throw new RuntimeException("Artifactory upload failed: " + statusCode);
	}
	httpClient.close();
}

def download(String url, String fileName, String user, String password, boolean verbose) throws ClientProtocolException, IOException  {
	RequestConfig requestConfig = RequestConfig
		.custom()
		.setSocketTimeout(timeoutMilliSeconds)
		.setConnectTimeout(timeoutMilliSeconds)
		.setCircularRedirectsAllowed(true)
		.setExpectContinueEnabled(true)
		.build();

	HttpGet get = new HttpGet(url);
	get.setConfig(requestConfig);

	HttpClientContext clientContext = HttpClientContext.create();
	CredentialsProvider provider = new BasicCredentialsProvider();
	UsernamePasswordCredentials credentials = new UsernamePasswordCredentials(user, password);
	provider.setCredentials(AuthScope.ANY, credentials);
	clientContext.setCredentialsProvider(provider);

	CloseableHttpClient httpClient = HttpClients.custom().useSystemProperties().build()
	
	HttpResponse response = httpClient.execute(get, clientContext);
	InputStream source = response.getEntity().getContent();
	FileOutputStream out = new FileOutputStream(fileName);
	byte[] buffer = new byte[8192];
	int read;
	while ((read = source.read(buffer)) != -1) {
		out.write(buffer, 0, read);
	}
	out.close();
	httpClient.close();
}


//Parsing the command line
def run(String[] cliArgs)
{

	def cli = new CliBuilder(usage: "ArtifactoryHelpers.groovy [options]", header: '', stopAtNonOption: false)
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

	if (opts.h)
	{
		cli.usage()
		System.exit(0)
	}
	
	if ( opts.fU) {
		upload(opts.u, opts.fU, opts.U, opts.P, opts.v)
	} else {
		download(opts.u, opts.fD, opts.U, opts.P, opts.v)
	}
}
