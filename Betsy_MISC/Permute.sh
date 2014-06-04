#! /bin/sh
#$ -pe omp 2
#$ -l h_rt=24:00:00
#$ -N OUT_PERMUTE
#$ -V

export OMPI_MCA_btl=tcp,sm,self
./Permute.netCDF.R

wait