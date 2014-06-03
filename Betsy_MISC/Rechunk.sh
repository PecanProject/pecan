#! /bin/sh
#$ -pe omp
#$ -l h_rt=24:00:00
#$ -N OUT_RECHUNK
#$ -V

export OMPI_MCA_btl=tcp,sm,self
cd /usr2/collab/ecowdery/pecan/Betsy_MISC/
./Rechunk.netCDF.R

wait