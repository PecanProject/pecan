#!/bin/bash
#SBATCH -J permute          # job name
#SBATCH -o permute-.o%j     # output and error file name (%j expands to jobID)
#SBATCH -n 8 -N 1           # total number of mpi tasks requested
                            # n = 8 to allow use of 4GB memory
#SBATCH -p serial      # queue (partition) -- normal, development, etc.
#SBATCH -t 01:00:00         # run time (hh:mm:ss) - 1.0 hours, for testing
#SBATCH --mail-user=dlebauer@gmail.com
#SBATCH --mail-type=begin  # email me when the job starts
#SBATCH --mail-type=end    # email me when the job finishes

module load gsl hdf5 parallel-netcdf/4.2.1.1

PWD="/scratch/02014/dlebauer/cruncep"
INPUT=${PWD}/out

## variables 1= start year 2 = end year
for y in `seq 1975 2010`; do
    echo "processing $y"
    T="$(date +%s)"
    time nccopy -m 4G -c 'time/1500,lat/1,lon/1' ${INPUT}/${y}.nc ${INPUT}/${y}ts.nc 
    
    echo "$(date) done processing $y"
    DT="$(($(date +%s)-T))"
    echo "Time: ${DT} seconds, ${SECONDS} total"
done

