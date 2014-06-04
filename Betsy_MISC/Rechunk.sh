#! /bin/sh
<<<<<<< HEAD
#$ -pe omp
=======
#$ -pe omp 2
>>>>>>> e1f11a40d8526ef1c88994436155acc65258a29e
#$ -l h_rt=24:00:00
#$ -N OUT_RECHUNK
#$ -V

export OMPI_MCA_btl=tcp,sm,self
<<<<<<< HEAD
cd /usr2/collab/ecowdery/pecan/Betsy_MISC/
=======
>>>>>>> e1f11a40d8526ef1c88994436155acc65258a29e
./Rechunk.netCDF.R

wait