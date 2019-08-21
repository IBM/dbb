#!/bin/sh
syntax() {
	echo "merge.sh <qualifier>"
	echo "  where <qualifier> is the qualifier to create datasets under."
	exit 16
}

num=$#
if [[ ${num} -eq 0 ]]; then
	syntax
fi
if [[ ${num} -gt 1 ]]; then
	syntax
fi
if [ "$1" = "-?" ]; then
	syntax
fi
prefix="$1"

echo "Dataset prefix: ${prefix}"

master="${prefix}.master"
new="${prefix}.new"
cmd="${prefix}.cmd"
merge="${prefix}.merge"

echo "Delete the sample datasets if they already exist"
ds="${master} ${new} ${cmd} ${merge}"
for d in ${ds}; do
	drm -f "${d}"
done

echo "Allocate the sample sequential datasets as FB 80 (the default for sequential datasets)"
for d in ${ds}; do
	dtouch -tseq "${d}"
done

echo "Write 3 records to master dataset"
decho    "Charles      Field       278 323 6045" "${master}"
decho -a "David        George      397 132 6025" "${master}"
decho -a "William      Young       178 333 5045" "${master}"

echo "Write 3 records to new dataset"
decho    "Emma         Hill        149 589 5045" "${new}"
decho -a "Sharon       Miller      153 232 6045" "${new}"
decho -a "Steve        Green       748 111 6025" "${new}"

echo "Write the command string to the command dataset"
decho " MERGE FORMAT=CH,FIELDS=(1,9,A)" "${cmd}"

args="MSGPRT=CRITICAL,LIST"
echo "The arguments to the SORT program for performing the merge are: ${args}"

echo "Run the SORT program to perform the merge."
echo "SORTIN01 DD name points to the master (old) dataset: ${master}"
echo "SORTIN02 DD name points to the new dataset: ${new}"
echo "SYSIN    DD name points to the commands dataset: ${cmd}"
echo "SORTOUT  DD name points to the resulting merged dataset: ${merge}"
echo "SYSOUT   DD name points to the console for output from the program"

mvscmd --pgm=SORT --args="${args}" --sortin01="${master}" --sortin02="${new}" --sysin="${cmd}" --sortout="${merge}" --sysout=*
rc=$?

if [[ "${rc}" -eq 0 ]]; then
	echo "Datasets successfully merged into: ${merge}"
else        
	echo "Datasets were not successfully merged. Return code: ${rc}"
fi
exit ${rc}

