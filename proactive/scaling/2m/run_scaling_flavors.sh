#!/bin/bash

trace=(1 2 3 4 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30 31 32 33 35 38 40)

capacity_tg=(0 0.1 0.2 0.3 0.4 0.5)

policy=(1 2 3 4 5 6)
ajd=(0 41 201 403 1009 2017 4031)

ecu=(1 7 8 3 6.5 6.5)
ghz=(NA 2.8 2.9 2.5 2.4 2.5)
mem=(1 3.75 3.75 3.75 8 15)

num_cores=`tail -n 30 /proc/cpuinfo | grep processor | awk '{print $3}'`
num_cores=`echo "$num_cores + 1" | bc`

slo_limit=1

for (( t=0; t<${#trace[@]}; t++ ))
do
	for (( p=0; p<${#policy[@]}; p++ ))
	do
		for (( c=0; c<${#ajd[@]}; c++ ))
		do
		
		     for (( y=0; y<${#capacity_tg[@]}; y++ ))
		     do
	
		          for (( i=0; i<${#ecu[@]}; i++ ))
			     do
     						
     				echo ${trace[$t]}
					counter=`ps -x | grep 'scaling-server-flavors' | grep -v grep | wc -l`
		                    
		               while [ $counter -ge $num_cores ]
	     			do
					     counter=`ps -x | grep 'scaling-server-flavors' | grep -v grep | wc -l`
                              sleep 10
					done
	    	          
		    	          nohup Rscript scaling-server-flavors.R ${trace[$t]} ${policy[$p]} ${ajd[$c]} ${ajd[$c]} normal normal acf_neg_i $slo_limit ${capacity_tg[$y]} ${capacity_tg[$y]} ${ecu[$i]} ${ghz[$i]} ${mem[$i]} > output/result-${trace[$t]}-${policy[$p]}-${ajd[$c]}-${ecu[$i]}-${ghz[$i]}-${mem[$i]}-norm.out &
		    	          nohup Rscript scaling-server-flavors.R ${trace[$t]} ${policy[$p]} ${ajd[$c]} ${ajd[$c]} normal ceiling_max acf_neg_i $slo_limit ${capacity_tg[$y]} ${capacity_tg[$y]} ${ecu[$i]} ${ghz[$i]} ${mem[$i]} > output/result-${trace[$t]}-${policy[$p]}-${ajd[$c]}-${ecu[$i]}-${ghz[$i]}-${mem[$i]}-cmax.out &
		    	                    
		    	          sleep 1
		    	                    
		          done
		     done
		done
	done 
done