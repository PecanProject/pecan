#This is the setup file for using initialize_SIPNET

cat("============ Setting up initialization project ============ \n")

#Load required data packages

cat("Loading required packages ... \n")
library(dplyr)
library(readr)

#Load required data

cat("Loading required data frames... \n")
trait_database <- load("master_data.RData")


#Load required functions

cat("Loading required functions ... \n")

source("average_by_trait.R")
source("find_same_class.R")
source("find_same_group_stats.R")
source("Class_Subclass_Mapping.R")
source("get_fallback_class_val.R")
source("get_fallback_value.R")
source("get_stats.R")
source("get_trait_value.R")
source("initialize_harvest.R")
source("initialize_planting.R")
source("initialize_SIPNET.R")
source("genus_mapping.R")
source("class_mapping.R")

cat("============ Setup complete! Ready to use! ============\n")
cat("Main function: initialize_planting and initialize_harvest \n")

cat("Example: intiailize_planting(34442, '10/25/2014', 'T19', 0.0005)\n")
cat("Example: initalize_harvest(34442, '10/25/2014', 'T19', 0.0005)\n")





