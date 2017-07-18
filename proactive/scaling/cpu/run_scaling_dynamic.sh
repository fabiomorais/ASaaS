#!/bin/bash

#trace=(14 8 20 15 35 21)  #af1
#trace=(16 2 24 31 38 26)  #af2
#trace=(11 3 19 4 32 1)   #af3
#trace=(10 40 23 17 33 18) #af4
#trace=(13 27 30 25 22 12) #af5

trace=(21)

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
				while [ $counter -ge 8 ]
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