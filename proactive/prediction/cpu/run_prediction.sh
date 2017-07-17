#!/bin/bash

trace=(1 2 3 4 8 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 30 31 32 33 35 38 40)

num_cores=`tail -n 30 /proc/cpuinfo | grep processor | awk '{print $3}'`
num_cores=`echo "$num_cores + 1" | bc`

for (( i=0; i<${#trace[@]}; i++ ))
do

        echo ${trace[$i]}
        Rscript server-parallel-predictor-LW.R ${trace[$i]} $num_cores normal normal > output/pred-${trace[$i]}-LW.out
        Rscript server-parallel-predictor-LR.R ${trace[$i]} $num_cores normal normal > output/pred-${trace[$i]}-LR.out
        Rscript server-parallel-predictor-AC.R ${trace[$i]} $num_cores normal normal > output/pred-${trace[$i]}-AC.out
        Rscript server-parallel-predictor-AR.R ${trace[$i]} $num_cores normal normal > output/pred-${trace[$i]}-AR.out
        Rscript server-parallel-predictor-ARIMA.R ${trace[$i]} $num_cores normal normal > output/pred-${trace[$i]}-ARIMA.out
        Rscript server-parallel-predictor-EN.R ${trace[$i]} $num_cores normal normal > output/pred-${trace[$i]}-EN.out

	   Rscript server-parallel-predictor-LW.R ${trace[$i]} $num_cores normal ceiling_max > output/pred-${trace[$i]}-LW.out
        Rscript server-parallel-predictor-LR.R ${trace[$i]} $num_cores normal ceiling_max > output/pred-${trace[$i]}-LR.out
        Rscript server-parallel-predictor-AC.R ${trace[$i]} $num_cores normal ceiling_max > output/pred-${trace[$i]}-AC.out
        Rscript server-parallel-predictor-AR.R ${trace[$i]} $num_cores normal ceiling_max > output/pred-${trace[$i]}-AR.out
        Rscript server-parallel-predictor-ARIMA.R ${trace[$i]} $num_cores normal ceiling_max > output/pred-${trace[$i]}-ARIMA.out
        Rscript server-parallel-predictor-EN.R ${trace[$i]} $num_cores normal ceiling_max > output/pred-${trace[$i]}-EN.out
done

