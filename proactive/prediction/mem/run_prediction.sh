#!/bin/bash

trace=(1 2 3 4 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30 31 32 33 35 38 40)

num_cores=`tail -n 30 /proc/cpuinfo | grep processor | awk '{print $3}'`
num_cores=`echo "$num_cores + 1" | bc`

pred_type=("normal" "ceiling_max")

for (( i=0; i<${#trace[@]}; i++ ))
do
	for (( j=0; j<${#pred_type[@]}; j++ ))
	do

	    echo "Prediction: trace "${trace[$i]} ${pred_type[$j]}

	    Rscript server-parallel-predictor-LW.R ${trace[$i]} $num_cores normal ${pred_type[$j]} > output/predmemory-${trace[$i]}-LW-${pred_type[$j]}.out
	    Rscript server-parallel-predictor-LR.R ${trace[$i]} $num_cores normal ${pred_type[$j]} > output/predmemory-${trace[$i]}-LR-${pred_type[$j]}.out
	    Rscript server-parallel-predictor-AC.R ${trace[$i]} $num_cores normal ${pred_type[$j]} > output/predmemory-${trace[$i]}-AC-${pred_type[$j]}.out
	    Rscript server-parallel-predictor-AR.R ${trace[$i]} $num_cores normal ${pred_type[$j]}  > output/predmemory-${trace[$i]}-AR-${pred_type[$j]}.out
	    Rscript server-parallel-predictor-ARIMA.R ${trace[$i]} $num_cores normal ${pred_type[$j]}> output/predmemory-${trace[$i]}-ARIMA-${pred_type[$j]}.out
	    Rscript server-parallel-predictor-EN.R ${trace[$i]} $num_cores normal ${pred_type[$j]} > output/predmemory-${trace[$i]}-EN-${pred_type[$j]}.out
	done
done
