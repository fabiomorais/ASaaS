#!/bin/bash

index=`echo "$1 - 1" | bc`

declare -a machines

machines[0]='1;2;15'    
machines[1]='3;4;16'    
machines[2]='8;10;17'   
machines[3]='11;12;18'  
machines[4]='13;14;19'  
machines[5]='20;21;22'  
machines[6]='23;24;25'  
machines[7]='26;27;30'  
machines[8]='31;32;33'  
machines[9]='35;38;40'  

traces=${machines[$index]}
trace=(${traces//;/ })

trace_type=("normal" "ceiling_max")

slo_limit=0.99

ncores=`tail -n 30 /proc/cpuinfo | grep processor | awk '{print $3}'`
ncores=`echo "$ncores + 1" | bc`

safety=("0" "0.1" "0.2" "0.3" "0.4" "0.5")
scenario=("0" "41" "1009" "2017" "4031")

ecu=(1) #7 8 3 6.5 6.5)
ghz=(NA) #2.8 2.9 2.5 2.4 2.5)
mem=(1) # 3.75 3.75 3.75 8 15)

for (( i=0; i<${#ecu[@]}; i++ ))
do
	for (( t=0; t<${#trace[@]}; t++ ))
	do
		for (( z=0; z<${#trace_type[@]}; z++ ))
		do	
			for (( j=0; j<${#scenario[@]}; j++ ))
			do	
				for (( c=0; c<${#safety[@]}; c++ ))
				do				

					counter=`ps -x | grep 'scaling-server-dynamic' | grep -v grep | wc -l`
					while [ $counter -ge $ncores ]
					do
					    counter=`ps -x | grep 'scaling-server-dynamic' | grep -v grep | wc -l`
					    sleep 10
					done

					echo "${trace[$t]} ${trace_type[$z]} ${scenario[$j]} ${safety[$c]} ${ecu[$i]} ${ghz[$i]} ${mem[$i]}"
											
					if [ "${scenario[$j]}" = "0" ]; then
			           	nohup Rscript scaling-server-dynamic.R ${trace[$t]} 1 NA F NA F 0 ${safety[$c]} $slo_limit ${trace_type[$z]} acf_neg_i ${ecu[$i]} ${ghz[$i]} ${mem[$i]} > output/dynamic-${trace[$t]}-1-NA-F-NA-F-0-${safety[$c]}-$slo_limit-${trace_type[$z]}-acf_neg_i-${ecu[$i]}-${ghz[$i]}-${mem[$i]}.out &
						sleep  1
								
			        else
						nohup Rscript scaling-server-dynamic.R ${trace[$t]} 1 NA F NA T ${scenario[$j]} ${safety[$c]} $slo_limit ${trace_type[$z]} acf_neg_i ${ecu[$i]} ${ghz[$i]} ${mem[$i]} > output/dynamic-${trace[$t]}-1-NA-F-NA-T-${scenario[$j]}-${safety[$c]}-$slo_limit-${trace_type[$z]}-acf_neg_i-${ecu[$i]}-${ghz[$i]}-${mem[$i]}.out &
						sleep  1
					fi

				done
			done		
		done
	done
done
