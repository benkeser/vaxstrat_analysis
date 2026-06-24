#!/bin/bash

PARTITION=$1
SETTING=$2
NSEEDS=$3

module purge
module load R/4.4.0

export PATH=/apps/R/4.4.0/bin:/usr/local/bin:/usr/bin:/usr/local/sbin

sbatch --array=1-$NSEEDS \
  --partition=$PARTITION \
  --nodes=1 \
  --ntasks-per-node=1 \
  --cpus-per-task=1 \
  --job-name=vegrowth_sim \
  --output=/projects/dbenkes/allison/vegrowth_analysis/scratch/${SETTING}_%A_%a.out \
  --export=SETTING=$SETTING,NSEEDS=$NSEEDS \
  --wrap "/apps/R/4.4.0/bin/Rscript R/run_analysis_contour.R"
