#!/bin/bash

trace=(1 2 3 4 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30 31 32 33 35 38 40)
trace_type=("normal" "ceiling_max")

slo_limit=0.99

safety=("0" "0.1" "0.2" "0.3" "0.4" "0.5")
scenario=("0" "41" "1009" "2017" "4031")

for (( t=0; t<${#trace[@]}; t++ ))
do

	for (( z=0; z<${#trace_type[@]}; z++ ))
	do
	
		for (( j=0; j<${#scenario[@]}; j++ ))
		do
		
			for (( c=0; c<${#safety[@]}; c++ ))
			do

				counter=`ps -x | grep 'scaling-server-dynamic' | grep -v grep | wc -l`
				while [ $counter -ge 4 ]
				do
				    counter=`ps -x | grep 'scaling-server-dynamic' | grep -v grep | wc -l`
				    sleep 10
				done
										
				if [ "${scenario[$j]}" = "0" ]; then
		           	nohup Rscript scaling-server-dynamic.R ${trace[$t]} 1 NA F NA F 0 ${safety[$c]} $slo_limit ${trace_type[$z]} acf_neg_i > output/dynamic-${trace[$t]}-1-NA-F-NA-F-0-${safety[$c]}-$slo_limit-${trace_type[$z]}-"acf_neg_i".out &
					sleep  5
							
		          else
					nohup Rscript scaling-server-dynamic.R ${trace[$t]} 1 NA F NA T ${scenario[$j]} ${safety[$c]} $slo_limit ${trace_type[$z]} acf_neg_i > output/dynamic-${trace[$t]}-1-NA-F-NA-T-${scenario[$j]}-${safety[$c]}-$slo_limit-${trace_type[$z]}-"acf_neg_i".out &
					sleep  5
				fi

			done		
		done
	done
done