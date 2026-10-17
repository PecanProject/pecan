#!/bin/bash -l
#$ -N salinas_ens_extract
#$ -pe omp 8
#$ -l h_rt=1:00:00
#$ -j y
#$ -o /projectnb/dietzelab/ccmmf/usr/akash/salinas_socs/ayushman_handoff/extract.log
module load R/4.4.0
cd /projectnb/dietzelab/ccmmf/usr/akash/salinas_socs/ayushman_handoff
Rscript extract_salinas_ensemble_output.R
