##' Benchmarking workflow
##' This could be a function that takes in either ensemble.id or bm.ensemble.id
##' Author: Betsy Cowdery


## --------- setup --------- ##

rm(list = setdiff(ls(), lsf.str()))  # clear environment except for sourced functions
# rm(list= ls()[!(ls() %in% c('objects you want to save'))] # clear environment except for ...
for (i in dbListConnections(PostgreSQL())) db.close(i) #close any stray database connections
options(digits = 20) # just to make things easier to read

require(PEcAn.all)
require(PEcAn.benchmark)
require(RPostgreSQL)
require(XML)
require(dplyr)

#db connection and host information

dbparms <- list(
  user = "bety",
  password = "bety",
  write = TRUE,
  driver = "PostgreSQL"
)
con     <- db.open(dbparms)
user_id <- 1000000003

# In case you want to re run calc.benchmark for an existing benchmark ensemble run - but if that's the case then you should probably start the workflow with the given bm.enseble id. This may just end up being for testing. 
new.run <-  FALSE

# Possible ensemble.ids to try
ensemble.id <- 1000003183 #WF ID 1000001544
ensemble.id <- 90000000001
ensemble.id <- 2000000017

## -------- definition : benchmark reference runs and benchmark ensemble id -------- #

ensemble <- db.query(paste("SELECT * FROM ensembles where id = ",ensemble.id,";"), con)
workflow <- db.query(paste("SELECT * FROM workflows where id = ",ensemble$workflow_id,";"), con)

# check if there is already a BRR for ensemble.id
BRR <- db.query(sprintf("SELECT * FROM reference_runs where settings = '%s' ", workflow$folder), con)
if(length(BRR) == 0) BRR <- create.BRR(ensemble.id, workflow, con)

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
