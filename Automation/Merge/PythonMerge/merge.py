from zoautil_py import MVSCmd, Datasets
from zoautil_py.types import DDStatement
import logging
import logging.config
logging.config.fileConfig('logging.conf')

# Create list of DD statements for MVSCmd
dd_statements = []
dd_statements.append(DDStatement(ddName="sortin01", dataset="USR.MVSCMD.DFSORT.MASTER"))
dd_statements.append(DDStatement(ddName="sortin02", dataset="USR.MVSCMD.DFSORT.NEW"))
dd_statements.append(DDStatement(ddName="sysin", dataset="USR.MVSCMD.DFSORT.CMD"))
dd_statements.append(DDStatement(ddName="sortout", dataset="USR.MVSCMD.DFSORT.MERGE"))
dd_statements.append(DDStatement(ddName="sysout", dataset="*"))

# Delete datasets if already exist
Datasets.delete("USR.MVSCMD.DFSORT.*")

# Create datasets
Datasets.create("USR.MVSCMD.DFSORT.MASTER", type="SEQ")
Datasets.create("USR.MVSCMD.DFSORT.NEW", type="SEQ")
Datasets.create("USR.MVSCMD.DFSORT.CMD", type="SEQ")
Datasets.create("USR.MVSCMD.DFSORT.MERGE", type="SEQ")

# Write command to USR.MVSCMD.DFSORT.CMD
Datasets.write("USR.MVSCMD.DFSORT.CMD", " MERGE FORMAT=CH,FIELDS=(1,9,A)")

# Write example text to USR.MVSCMD.DFSORT.MASTER
Datasets.write("USR.MVSCMD.DFSORT.MASTER", "Chang Joe 278 232 6043")
Datasets.write("USR.MVSCMD.DFSORT.MASTER", "DeBeer Jo 348 132 6023", append=True)
Datasets.write("USR.MVSCMD.DFSORT.MASTER", "White Belinda 178 222 5043", append=True)

# Write example text to USR.MVSCMD.DFSORT.NEW
Datasets.write("USR.MVSCMD.DFSORT.NEW", "Doe Jane 878 222 5043")
Datasets.write("USR.MVSCMD.DFSORT.NEW", "Smith Joe 778 232 6043", append=True)
Datasets.write("USR.MVSCMD.DFSORT.NEW", "Smyth Jo 748 132 6023", append=True)

rc = MVSCmd.execute(pgm="sort", args="MSGPRT=CRITICAL,LIST", dds=dd_statements)

if rc == 0:
	print(Datasets.read("USR.MVSCMD.DFSORT.MERGE"))
