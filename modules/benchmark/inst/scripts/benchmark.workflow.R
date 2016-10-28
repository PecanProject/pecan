##' Benchmarking workflow
##' Only use this if using and existing run (ie settings$new_run == FALSE)
##' Author: Betsy Cowdery

## ----- setup: these are not necessary but make testing easier ----- ##
rm(list = setdiff(ls(), lsf.str()))  # clear environment except for sourced functions
# rm(list= ls()[!(ls() %in% c('objects you want to save'))] # clear environment except for ...
for (i in dbListConnections(PostgreSQL())) db.close(i) #close any stray database connections
options(digits = 10) # just to make things easier to read
## ----- setup: these are not necessary but make testing easier ----- ##

library(PEcAn.all)
library(PEcAn.benchmark) # Should we add PEcAn.benchmark to PEcAn.all?
library(RPostgreSQL)
library(XML)
library(dplyr)
library(PEcAn.visualization) # Really shouldn't need this but I haven't reconsiled this and PEcAn.DB

bety <- betyConnect("web/config.php")

# Start with a benchmarking settings xml file that would be created though the interface 

# ----- Pick a settings file -----#
# WORKING: 1 variable, 1 metric, 1 site, 1 model 
#settings.file <- "modules/benchmark/inst/scripts/bm.1var.1metric.1site.1model.xml"

# WORKING: 2 variables, 2 metric, 1 site, 1 model 
settings.file <- "modules/benchmark/inst/scripts/bm.2var.2metric.1site.1model.xml"

# NOT WORKING: 2 variables, 2 metric, 2 site, 1 model (multisettings object)
# settings.file <- "modules/benchmark/inst/scripts/bm.2var.2metric.2site.1model.xml"
# --------------------------------#

settings <- read.settings(settings.file)

# For testing (make sure new_run is FALSE)
settings$benchmark$new_run <- FALSE

settings <- papply(settings, read.settings.RR)
settings <- do.conversions(settings)
settings <- papply(settings, function(x) create.benchmark(x, bety))
results <- papply(settings, function(x) calc.benchmark(x, bety))

str(results)
