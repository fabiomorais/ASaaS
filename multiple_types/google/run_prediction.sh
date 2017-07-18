#!/bin/bash

nfile=$1
file="output/27042016T212529/conf/"$nfile"_sub_jobs.txt"
nrow=`cat $file | wc -l`

i=1
ncores=7
adj=42

while read p; do
  job=$p

  echo "$i/$nrow"
  Rscript server-parallel-predictor-AR-acf.R $job $ncores $adj >> "output/"$nfile"_tmp_ar.out"
  #Rscript server-parallel-predictor-AR.R $job $ncores >> "output/"$nfile"_tmp_ar.out"

  i=`echo "$i + 1" | bc`

done <$file