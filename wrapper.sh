#!/bin/bash
hm=$(pwd)
main="main.r"
for f2 in $(ls -vd */); do
	cd $f2
	echo $f2
	if [ ! -e "totsum.txt" ]; then
		Rscript $main
	fi
	cd ../
done
