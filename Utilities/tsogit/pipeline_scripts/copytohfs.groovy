import com.ibm.dbb.build.*

def pathnam = args[0]
def pdsnam  = args[1]
def memnam  = args[2]

println("Source directory : $pathnam")
println("Target pds       : $pdsnam")
println("member name      : $memnam")

new CopyToHFS().dataset(pdsnam).member(memnam)file(new File(pathnam)).copy()