#!/bin/bash

#trace=(14 12 27) #5.141
#trace=(16 8 18)  #5.142
#trace=(11 2 20)  #5.143
#trace=(10 3 24)  #2.166
#trace=(13 40 19) #5.145
#trace=(31 35 21) #5.170
#trace=(15 26 22) #5.171
#trace=(1 25 33)  #5.172
#trace=(30 17 32) #5.173
#trace=(23 4 38)  #5.174

#trace=(14 12 27 23 4 38)
#trace=(16 8 18 30 17 32)
#trace=(11 2 20 1 25 33)
#trace=(10 3 24 15 26 22)
#trace=(13 40 19 31 35 21)

safety=("0" "0.1" "0.2" "0.3" "0.4" "0.5")
policy=(4 6) #(1 2 3 4 5 6)
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

    				#nohup Rscript scaling-server-flavors.R ${trace[$t]} ${policy[$p]} ${ajd[$a]} normal normal acf_neg_i $slo_limit ${safety[$x]} $instance_size > output/scaling-${trace[$t]}-${policy[$p]}-${ajd[$a]}-${safety[$x]}.out &
					  nohup Rscript scaling-server-flavors.R ${trace[$t]} ${policy[$p]} ${ajd[$a]} normal ceiling_max acf_neg_i $slo_limit ${safety[$x]} $instance_size > output/scaling-${trace[$t]}-${policy[$p]}-${ajd[$a]}-${safety[$x]}.out &

            sleep 1
			done
		done
	done 
done
