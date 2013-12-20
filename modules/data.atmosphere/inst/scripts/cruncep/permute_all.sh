#!/bin/bash
#SBATCH -J cruncep           # job name
#SBATCH -o cruncep.o%j       # output and error file name (%j expands to jobID)
#SBATCH -n 32                # total number of mpi tasks requested
#SBATCH -p development       # queue (partition) -- normal, development, etc.
#SBATCH -t 01:00:00          # run time (hh:mm:ss) - 1.0 hours
#SBATCH --mail-user=dlebauer@gmail.com
#SBATCH --mail-type=begin  # email me when the job starts
#SBATCH --mail-type=end    # email me when the job finishes

module load gsl
module load hdf5
module load netcdf/4.2.1.1
PWD="/scratch/02014/dlebauer/cruncep"
INPUT=${PWD}
OUTPUT=${PWD}/out

## variables 1= start year 2 = end year
for y in `seq $1 $2`; do
    echo "$(date) processing $y"
    for dir in lwdown press qair rain swdown tair uwind vwind;
      do
      v="${INPUT}/${dir}"
      for f in ${v}/*${y}.nc; do
	  in=${f}
	  out=${v}/${dir}${y}ts.nc
	time nccopy -w -c 'time/1500,lat/10,lon/10' ${in} ${out}
      done
    done
    echo "$(date) done processing $y"
done

