#!/bin/sh
#$ -pe mpi_16_tasks_per_node 64
#$ -l h_rt=96:00:00
#$ -N QED2
#$ -V
export OMPI_MCA_btl=tcp,sm,self
cd /usr3/graduate/ttviskar/DART/Kodiak/models/FIA/work
mpirun -np 64 filter
#./ed_2.1-opt
wait
