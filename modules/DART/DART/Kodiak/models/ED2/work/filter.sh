#!/bin/sh
#$ -pe omp 2
#$ -l h_rt=24:00:00
#$ -N toni_test 
#$ -V
cd /net/casfsb/vol/ssrchome/active_users/ttviskar/DART/Kodiak/models/ED2/work
./filter
wait
