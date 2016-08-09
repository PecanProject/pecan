##' Benchmarking workflow
##' This could be a function that takes in either ensemble.id or bm.ensemble.id
##' Author: Betsy Cowdery


## --------- setup --------- ##

rm(list = setdiff(ls(), lsf.str()))  # clear environment except for sourced functions
# rm(list= ls()[!(ls() %in% c('objects you want to save'))] # clear environment except for ...
for (i in dbListConnections(PostgreSQL())) db.close(i) #close any stray database connections
options(digits = 4) # just to make things easier to read

library(PEcAn.all)
library(PEcAn.benchmark)
library(RPostgreSQL)
library(XML)
library(dplyr)

#db connection and host information

dbparms <- list(
  user = "bety",
  password = "bety",
  host = "psql-pecan.bu.edu",
  dbname = "bety",
  write = TRUE,
  driver = "PostgreSQL"
)
con     <- db.open(dbparms)
user_id <- 1000000003 # Betsy's ID 

# In case you want to re-run calc.benchmark for an existing benchmark ensemble run - but if that's the case then you should probably start the workflow with the given bm.enseble id. This may just end up being for testing. 
new.run <-  FALSE

# Example ensemble.ids to try
ensemble.id <- 1000003655

## -------- definition : benchmark reference runs and benchmark ensemble id -------- #

ensemble <- db.query(paste("SELECT * FROM ensembles where id = ",ensemble.id,";"), con)
workflow <- db.query(paste("SELECT * FROM workflows where id = ",ensemble$workflow_id,";"), con)

# check if there is already a BRR for ensemble.id
BRR <- db.query(sprintf("SELECT * FROM reference_runs where settings = '%s' ", workflow$folder), con)
if(length(BRR) == 0) BRR <- create.BRR(ensemble.id, workflow, con)

# If there is not an existing BRR, there are parts of the process that aren't automated yet.
# Because you are creating a new reference run you will need to:
# 1) Create records for new benchmarking data sources using the formats, inputs and dbfiles tables. 
# 2) Create new benchmark etries for variables of interest in the new data sources.

# # CDIAC_ORNL = 1000000651
# # NPP = 411
# db.query(paste(
#   "INSERT INTO benchmarks (input_id, site_id, variable_id, created_at, updated_at)",
#   "VALUES ( 1000000651 ,", workflow$site_id, ", 411 , NOW(), NOW() ) RETURNING * "), con)
# benchmarks <- db.query("SELECT * FROM benchmarks WHERE input_id =  1000000651 and site_id=666 and variable_id=411",con)
# # 3) Relate benchmarks with the reference run in the benchmarks_benchmarks_reference_runs table.
# benchmarks_BRR <- db.query(paste(
#   "INSERT INTO benchmarks_benchmarks_reference_runs (benchmark_id, reference_run_id)",
#   "VALUES (",benchmarks$id,",",BRR$id,") RETURNING * "),con)

bm.ensembles <- db.query(sprintf("SELECT * FROM benchmarks_ensembles WHERE reference_run_id = %0.f ", BRR$id),con)

bm.host <- db.query(sprintf("SELECT w.hostname, e.id FROM workflows as w JOIN ensembles as e ON w.id = e.workflow_id JOIN benchmarks_ensembles as be ON e.id = be.ensemble_id WHERE be.reference_run_id = %0.f ", BRR$id),con)
local <- which(bm.host$hostname == fqdn())

if(length(local) == 0 | new.run == TRUE){
  bm.ensemble <- start.bm.ensemble(BRR, con)
}else{
  # how do you choose which ref run to use? Maybe through shiny? For now, take the 1st one...
  bm.ensemble <- filter(bm.ensembles, ensemble_id == bm.host$id[local][1])
}

## -------- evaluation: calculations and reporting -------- #

results <- calc.benchmark(bm.ensemble, con)

# Ultimately this will launch the shiny app
# print(source('modules/benchmark/R/prototype.shiny.2.R')$value)
