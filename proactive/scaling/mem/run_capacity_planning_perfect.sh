#!/bin/bash

trace=(1 2 3 4 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30 31 32 33 35 38 40)
filter=("normal" "ceiling_max")

slo_limit=1

for (( i=0; i<${#trace[@]}; i++ ))
do
	echo "${trace[$i]}"

	for (( j=0; j<${#filter[@]}; j++ ))
	do
		Rscript capacity-planning-perfect.R ${trace[$i]} 1 normal ${filter[$j]} acf_neg_i $slo_limit
	done
done