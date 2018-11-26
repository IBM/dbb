def pgmName = gitPgmNameParm

try {
    node('INFSTONE-TSDEV1') {

		stage('Clean WorkSpace') {
	    	// clean up workspace
	    	deleteDir()
	    }
	    stage('Git Checkout') {
	    	//checkout code
	    	scminfo = codeCheckout()
	    	echo "${scminfo.GIT_COMMIT}"
	    	echo "${scminfo.GIT_BRANCH}"
	    	echo "${scminfo.GIT_AUTHOR_EMAIL}"
	    }
	    stage ('Execute Test Copy of TSO GIT'){
		    //execute copy to test
	    	copytoTest()
		}
    	stage ('Execute Clone Test'){
    		//execute test cases in batch
    		executeTestCases()
    	}
	    stage ('Execute Prod Copy of TSO GIT'){
		    //execute copy to prod
	    	if (scminfo.GIT_BRANCH == 'origin/master')
	    		copytoProd()
	    		else
	                println("Skipping Copy to Prod step")
	    
		}
    }
}

catch (e) {
    currentBuild = 'UNSTABLE'
    throw e
}
finally {
    if (currentBuild == 'UNSTABLE' || currentBuild == 'FAILURE') {
        echo "Fail"
    } else {
        echo "Success"
    }
}

//git checkout function code
def codeCheckout() {
    	checkout([$class: 'GitSCM', 
    	          branches: [[name: '*/']], 
    	          doGenerateSubmoduleConfigurations: false, 
    	          extensions: [[$class: 'WipeWorkspace'],
    	                       [$class: 'LocalBranch', localBranch: ''],
    	                       [$class: 'PreBuildMerge', options: [mergeRemote: 'origin', mergeTarget: 'master']]
    	          ],
    	          submoduleCfg: [], 
    	          userRemoteConfigs: [[credentialsId: 'b3ac5dcc-dc96-4910-ace3-3a9d9fcfe4c9', url: 'git@sfgitlab.opr.statefarm.org:mainframecd/TSO-GIT.git']]])
    	
}

// Execute test cases 
def executeTestCases() {
	job2=build job:'tsogit_test',parameters:[string(name:'gitPgmNameParm',value:gitPgmNameParm)]
	        //Aggregatedownstreamjobsloginthepipeline
	        //Requiremethodsignatureapproval
	        log=Jenkins.getInstance().getItemByFullName('tsogit_test').getBuildByNumber(job2.getNumber()).getLog()
	        echo"${log}"
}

def copytoTest(){
	//TSDEV test SUFFIX for TSO GIT
	def testhlq = 'C03058.MFCD.GIT.TEST'
	job1=build job:'tsogit1',parameters:[string(name:'gitPgmNameParm',value:gitPgmNameParm),
	                                              string(name:'suffixParm', value:testhlq)]
	        //Aggregatedownstreamjobsloginthepipeline
	        //Requiremethodsignatureapproval
	        log=Jenkins.getInstance().getItemByFullName('tsogit1').getBuildByNumber(job1.getNumber()).getLog()
	        echo"${log}"
}

def copytoProd(){
	//TSDEV prod SUFFIX for TSO GIT
	def prodhlq = 'C03058.MFCD.GIT'
	job3=build job:'tsogit1',parameters:[string(name:'gitPgmNameParm',value:gitPgmNameParm),
	                                              string(name:'suffixParm', value:prodhlq)]
	        //Aggregatedownstreamjobsloginthepipeline
	        //Requiremethodsignatureapproval
	        log=Jenkins.getInstance().getItemByFullName('tsogit1').getBuildByNumber(job3.getNumber()).getLog()
	        echo"${log}"
}
