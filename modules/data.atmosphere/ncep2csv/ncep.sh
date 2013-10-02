#!/bin/bash                                                                     
#$ -S /bin/bash                                                                 
source /etc/profile

module load R/3.0.1

R CMD BATCH --no-save "--args $1 $2" /home/dlebauer/met/ncep/Globalmet.R $1$2.log

