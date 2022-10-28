#! /bin/bash -l
#$ -N SDAprep
#$ -l buyin
#$ -o /projectnb/dietzelab/ahelgeso/Forecast_Scripts/rscript_output/dataprep.log
#$ -m ea
#$ -j y
module load R/4.1.1
Rscript '/projectnb/dietzelab/ahelgeso/pecan/modules/assim.sequential/inst/WillowCreek/SDA_Workflow_LAI.R'