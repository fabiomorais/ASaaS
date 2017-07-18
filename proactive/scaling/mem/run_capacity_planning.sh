#!/bin/bash

trace=(1 2 3 4 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30 31 32 33 35 38 40)

num_cores=`tail -n 30 /proc/cpuinfo | grep processor | awk '{print $3}'`
num_cores=`echo "$num_cores + 1" | bc`

slo_limit=1

for (( i=0; i<${#trace[@]}; i++ ))
do
	echo "${trace[$i]}"

	Rscript capacity-planning-server.R ${trace[$i]} $num_cores normal normal acf_neg_i $slo_limit
	Rscript capacity-planning-server.R ${trace[$i]} $num_cores normal ceiling_max acf_neg_i $slo_limit

done
