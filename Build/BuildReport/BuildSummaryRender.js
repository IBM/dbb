function render()
{		
	if (this.readyState == 4 && (this.status == 200 || this.status == 0))
	{   
		var prevFile = "";
		var nextFile = "";
		var counter = 1;    	
		var tableHtml = "";
		var jenkinsBuildHtml = "";
		var cRecord;
		var jenkinsWorkspace;
		var jenkinsJobUrl;
		var versionInfoHtml = "";
		var dependencySetCache = new Object();
		var buildReport = JSON.parse(this.responseText);
		/** Add a new header to display the Git commit hash value */
		tableHtml += "<table><tr><th></th><th>File</th><th>Git Hash</th><th>Commands</th><th>RC</th><th>Data Sets</th><th>Outputs</th><th>Deploy Type</th><th>Logs</th></tr>";
		for (x in buildReport.records)
		{        	        	
			var record = buildReport.records[x];
			if (record.type == "EXECUTE")
			{        		        		
				nextFile = record.file; 

				//If we are coming across different file for the current EXECUTE record
				//then display the last processing consolidated record and generate a new
				//consolidated record.
				if (prevFile == "" || prevFile != nextFile)
				{        			
					if (cRecord != null)
					{	        
						tableHtml += renderExec(counter, cRecord, dependencySetCache);
						counter++;
					}

					//Reset the consolidate record
					cRecord = new Object();        			        			
					cRecord.file = record.file;        			
					cRecord.executors = [];
					cRecord.rcs = [];
					cRecord.datasets = [];
					cRecord.outputs = [];
					cRecord.deployTypes = [];
					cRecord.logs = [];
					cRecord.logFiles = []; /* maintain this to avoid having duplicated log files for each file */
					cRecord.gitHash = "";   /** Reset the commit Git hash value before parsing the next record */
				}
				var numExecutors = cRecord.executors.length;
				cRecord.executors[numExecutors] = record.command;
				cRecord.rcs[numExecutors] = record.rc;
				cRecord.outputs[numExecutors] = "";
				cRecord.deployTypes[numExecutors] = "";
				for (z in record.outputs)
				{
					var output = record.outputs[z];
					if (cRecord.outputs[numExecutors].length > 0)
					{
						cRecord.outputs[numExecutors] += "<br>";
						cRecord.deployTypes[numExecutors] += "<br>";
					}	
					cRecord.outputs[numExecutors] += output.dataset;
					if (output.deployType != null)
						cRecord.deployTypes[numExecutors] += output.deployType;
					else cRecord.deployTypes[numExecutors] += "&nbsp;";
						
				}
				cRecord.datasets[numExecutors] = "";
				for (m in record.datasets)
				{
					var dataset = record.datasets[m];
					if (cRecord.datasets[numExecutors].length > 0)
						cRecord.datasets[numExecutors] += "<br>";
					cRecord.datasets[numExecutors] += dataset;	
				}
				cRecord.logs[numExecutors] = "";
				for (n in record.logs)
				{        			
					var log = record.logs[n];
					var duplicateLog = false;
					for (p in cRecord.logFiles)
					{
						if (log == cRecord.logFiles[p])
						{
							duplicateLog = true;
							break;
						}	        					
					}

					if (!duplicateLog)
					{	
						if (cRecord.logs[numExecutors].length > 0)
							cRecord.logs[numExecutors] += "<br>";
							var logName = log.split(/[\\/]/g).pop();
							if (jenkinsJobUrl != null && jenkinsWorkspace != null && log.startsWith(jenkinsWorkspace))
							{
								var logLink = "<a href='" + jenkinsJobUrl + "ws" + log.slice(jenkinsWorkspace.length) + "/*view*/' target='_blank'>" + logName + "</a>";
								cRecord.logs[numExecutors] = logLink;
							}
							else
							{	
								cRecord.logs[numExecutors] += logName;
							}	
							cRecord.logFiles[cRecord.logFiles.length] = log;
					}
				}
				prevFile = nextFile;
			}
			else if (record.type == 'JENKINS')
			{
				jenkinsBuildHtml += "<div id='jenkins'><p><h3>Jenkins Build</h3>";
				jenkinsBuildHtml += "<p><table><tr><td class='label'>Project: </td><td><a href='" + record.jobUrl + "'>" + record.jobName + "</a></td></tr>";
				jenkinsBuildHtml += "<tr><td class='label'>Build: </td><td><a href='" + record.buildUrl + "'>" + record.buildNumber + "</a></td></tr>";        		
				jenkinsBuildHtml += "<tr><td class='label'>Workspace: </td><td>" + record.workspace + "</td></tr></table></div>";
				if (jenkinsWorkspace == null)
					jenkinsWorkspace = record.workspace;
				if (jenkinsJobUrl == null)
					jenkinsJobUrl = record.jobUrl;
			}
			else if (record.type == 'DEPENDENCY_SET')
			{        	
				if (record.dependencySet != null && record.dependencySet.length > 0)
					dependencySetCache[record.file] = record.dependencySet;		        		
			}
			else if (record.type == 'VERSION')
			{
				versionInfoHtml = "<div id='VersionInfo'><p><h3>Toolkit Version:</h3></p>";
				versionInfoHtml += "<p><table><tr><td class='label'>Version: </td><td>" + record.version + "</td></tr>";
				versionInfoHtml += "<tr><td class='label'>Build: </td><td>" + record.build + "</td></tr>";
				versionInfoHtml += "<tr><td class='label'>Date: </td><td>" + record.date + "</td></tr></table></div>"        		
			}
			/** Parse the Git commit hash value for each file */
			else if (record.type == 'GIT_HASH')
			{
				cRecord.gitHash = record.commitHashValue 
			}			
		}

		if (cRecord != null)
		{
			tableHtml += renderExec(counter, cRecord, dependencySetCache);
			counter++;
		}	

		tableHtml += "</table>"; 
		var mainHtml = "<div id='main'>";
		var buildSummaryHtml = "<div id='buildSummary'>";       
		buildSummaryHtml += "<p><h3>Build Summary</h3><p>Number of files being built: " + (counter-1);
		buildSummaryHtml += "<p>" + tableHtml + "</div>";
		mainHtml += versionInfoHtml;
		mainHtml += jenkinsBuildHtml;
		mainHtml += buildSummaryHtml;
		mainHtml += "</div>";
		document.getElementById("main").innerHTML = mainHtml;
	}  
}

function renderExec(counter, cRecord, dependencySetCache)
{	
	var tr  = "<tr>";
	if (counter % 2 == 0)
		tr = "<tr class='even'>";
	var rowHtml = tr;
	rowHtml += "<td rowspan='" + cRecord.executors.length + "'>" + counter + "</td><td rowspan='" + cRecord.executors.length + "'>" + cRecord.file;
	var dependencySet = dependencySetCache[cRecord.file];
	if (dependencySet != null)
	{        					
		var depId = "dep_" + counter;
		var showDepId = "showDep_" + counter;
		rowHtml += "<div>";
		rowHtml += "<div id='" + depId + "' style='display:none' class='dep'>";
		rowHtml += "<ul>";
		for (q in dependencySet)
		{
			rowHtml += "<li>";
			var dependency = dependencySet[q];
			if (dependency.resolved)
				rowHtml += dependency.file;
			else
				rowHtml += dependency.lname;
			rowHtml += "&nbsp;<sub class='depType'>" + dependency.category + "</sub>";
			rowHtml += "</li>";        						
		}
		rowHtml += "</div>";
		rowHtml += "<a id='" + showDepId + "' class='bottomRight' href='javascript:onclick=toggle(\"" + depId + "\",\"" + showDepId + "\")'>Show Dependencies</a>";        					
		rowHtml += "</div>";       					
	}	
	rowHtml += "</td>";
	/** Generate a HTML column to display the Git commit hash value for each file */
	rowHtml += "<td rowspan='" + cRecord.executors.length + "'>" + cRecord.gitHash + "</td>";
	var cRecordCount = 0;
	for (y in cRecord.executors)
	{
		rowHtml += "<td>" + cRecord.executors[y] + "</td><td>" + cRecord.rcs[y] + "</td><td>" + cRecord.datasets[y] + "</td><td>";
		rowHtml += cRecord.outputs[y] + "</td><td>" + cRecord.deployTypes[y] + "</td><td>" + cRecord.logs[y] + "</td>";		
		if (cRecordCount < cRecord.executors.length)
			rowHtml += "</tr>" + tr;
		cRecordCount++;
	}        				
	rowHtml += "</tr>";
	return rowHtml;
}