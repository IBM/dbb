## TSO GIT Overview  
The TSO GIT utility is a Set if REXX panels which issues JCL to perform basic GIT functionality. Below are pseudo git commands used by the utility for each funtion. 

## Assumptions:
- JCL executes under USER.
- User has access to write to HLQ.
- Program name is needed to change to the correct directory for copy.
- User can't switch between branches in the same repo.
- User has completed the TSO GITSETUP with does setup of Git for z/OS environment variables in the users .profile
- SSH key creation will email users their public key which must be added to the users GitLab profile.
- SSH key validation will test the key against GitLab.
- PDSs are deleted and reallocated everytime a new clone is performed to avoid mixing up artifacts from different repos.
- The advanced GIT option screen brings up JCL skeletons for the user to edit and submit.
- The GIT workspace is the users home directory on USS (u/<user-alias>/$gitproject/$program/..) 
- Git command line can be used directly against the users repo using OMVS or Putty 

##### GIT CLONE 
// change to users home directory  
cd /u/$alias

//remove the directory to ensure a clean clone  
rm -fR  $gitproject

// perform git clone  
git clone $gitgroup/$gitproject

//change to the git project folder   
cd /u/$alias/$gitproject

//checkout the branch name  
git checkout -b $branchname

//copy each member listed in the git repo to the matching PDS     
import com.ibm.dbb.build.*  
def pathnam = args[0]  
def pdsnam  = args[1]  
def memnam  = args[2]  
new CopyToPDS().file(new File(pathnam)).dataset(pdsnam).member(memnam).copy() 


##### GIT PULL
// change to users git repo directory  
cd /u/$alias/$gitproject

//copy each member listed in the PDS to the git repo folder   
import com.ibm.dbb.build.*  
def pathnam = args[0]  
def pdsnam  = args[1]  
def memnam  = args[2]  
new CopyToHFS().dataset(pdsnam).member(memnam)file(new File(pathnam)).copy()  

//add modified artifacts to git index  
git add . 

//perform a git pull to merge code in the default branch that was checked out initially  
git pull

//copy each member listed in the git repo to the matching PDS   
import com.ibm.dbb.build.*  
def pathnam = args[0]  
def pdsnam  = args[1]  
def memnam  = args[2]  
new CopyToPDS().file(new File(pathnam)).dataset(pdsnam).member(memnam).copy() 


##### GIT PUSH
// change to users git repo directory  
cd /u/$alias/$gitproject

//copy each member listed in the PDS to the git repo folder   
import com.ibm.dbb.build.*  
def pathnam = args[0]  
def pdsnam  = args[1]  
def memnam  = args[2]  
new CopyToHFS().dataset(pdsnam).member(memnam)file(new File(pathnam)).copy()  

//add modified artifacts to git index  
git add . 

//commit local changes   
git commit -m '$commitmessage'

//perform a git push to upload local artifacts to server  
git push origin $branchname

1
