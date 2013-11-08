#!/bin/bash                                                                     
#$ -S /bin/bash                                                                 
source /etc/profile

module load R/3.0.1

R CMD BATCH --no-save "--args $1 $2" /home/dlebauer/met/ncep/met2csv.R $1$2met2csv.log

