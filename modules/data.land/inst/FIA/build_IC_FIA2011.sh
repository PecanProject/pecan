#!/bin/bash -l
#$ -N build_ic_fia2011
#$ -pe omp 1
#$ -l h_rt=4:00:00
#$ -l mem_per_core=16G
#$ -l buyin
#$ -P dietzelab
#$ -o /projectnb/dietzelab/cwebb16/SDA/build_IC_FIA2011.log
#$ -e /projectnb/dietzelab/cwebb16/SDA/build_IC_FIA2011.err
#$ -j y

Rscript /projectnb/dietzelab/cwebb16/SDA/build_IC_FIA2011.R
