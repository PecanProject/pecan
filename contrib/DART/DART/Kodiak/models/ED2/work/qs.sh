#!/bin/sh
#$ -pe omp 4
#$ -l h_rt=24:00:00
#$ -N QED2
#$ -V
export OMPI_MCA_btl=tcp,sm,self
cd /usr3/graduate/ttviskar/DART/Kodiak/models/ED2/work
csh mfilter.csh
#mpirun -np 4 filter
#./ed_2.1-opt
wait
