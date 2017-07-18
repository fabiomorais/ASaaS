#!/bin/bash

trace=(1 2 3 4 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30 31 32 33 35 38 40)

safety=("0" "0.1" "0.2" "0.3" "0.4" "0.5")
policy=(1 2 3 4 5 6)
ajd=(0 41 201 403 1009 2017 4031)
slo_limit=1
instance_size=1

num_cores=`tail -n 30 /proc/cpuinfo | grep processor | awk '{print $3}'`
num_cores=`echo "$num_cores + 1" | bc`

for (( t=0; t<${#trace[@]}; t++ ))
do
	for (( p=0; p<${#policy[@]}; p++ ))
	do
		for (( a=0; a<${#ajd[@]}; a++ ))
		do
			for (( x=0; x<${#safety[@]}; x++ ))
			do
			        counter=`ps -x | grep 'scaling-server' | grep -v grep | wc -l`
					while [ $counter -ge $num_cores ]
					do
				        counter=`ps -x | grep 'scaling-server' | grep -v grep | wc -l`
				        sleep 10
    				done

    				nohup Rscript scaling-server-flavors.R ${trace[$t]} ${policy[$p]} ${ajd[$a]} normal normal acf_neg_i $slo_limit ${safety[$x]} $instance_size > output/scaling-${trace[$t]}-${policy[$p]}-${ajd[$a]}-${safety[$x]}.out &
				nohup Rscript scaling-server-flavors.R ${trace[$t]} ${policy[$p]} ${ajd[$a]} normal ceiling_max acf_neg_i $slo_limit ${safety[$x]} $instance_size > output/scaling-${trace[$t]}-${policy[$p]}-${ajd[$a]}-${safety[$x]}.out &

                    sleep 5
			done
		done
	done 
done
