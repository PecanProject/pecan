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
module load netcdf
module load nco/4.3.0
PWD="/scratch/02014/dlebauer/cruncep"
INPUT=${PWD}
OUTPUT=${PWD}/out

## variables 1= start year 2 = end year
for y in `seq $1 $2`; do
    echo "$(date) processing $y"
    for dir in lwdown press qair rain swdown tair uwind vwind;
      do
      v="${INPUT}/${dir}"
      echo $v
      for f in ${v}/*${y}.nc; do
	ionice -n 2 ncks -A ${f} ${OUTPUT}/${y}.nc
      done
    done
    echo "$(date) done processing $y"
done

echo "$(date) concatting all years"
nohup ionice -n 2 ncrcat -n 110,3,1 1901.nc all.nc 
echo "$(date) done concatting all years"
