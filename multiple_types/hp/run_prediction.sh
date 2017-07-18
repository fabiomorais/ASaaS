#!/bin/bash

trace=(1 2 15 3 4 16 8 10 17 11 12 18 13 14 19 20 21 22 23 24 25 26 27 30 31 32 33 35 38 40)

num_cores=`tail -n 30 /proc/cpuinfo | grep processor | awk '{print $3}'`
num_cores=`echo "$num_cores + 1" | bc`

for (( i=0; i<${#trace[@]}; i++ ))
do
     echo ${trace[$i]}
     Rscript server-parallel-predictor-AR.R ${trace[$i]} $num_cores >> "output/"${trace[$i]}"_tmp_ar.out"
     Rscript server-parallel-predictor-AR-acf.R ${trace[$i]} $num_cores 42 >> "output/"${trace[$i]}"_tmp_ar.out"
     Rscript server-parallel-predictor-AR-acf.R ${trace[$i]} $num_cores 84 >> "output/"${trace[$i]}"_tmp_ar.out"
     Rscript server-parallel-predictor-AR-acf.R ${trace[$i]} $num_cores 168 >> "output/"${trace[$i]}"_tmp_ar.out"
     
done

