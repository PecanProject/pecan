## Code to insert ForestGEO species list
## Mike Dietze
## some of the code here is borrowed from Alexey's import-try 04.match.species
library(data.table)
library(PEcAn.DB)
library(RPostgreSQL)

## Load raw data
## File generated 11/14/16 by cut-and-paste from web query
## http://ctfs.si.edu/webatlas/neotropicaltree/
## then saved as CSV. CTFS needs a better API
sppFile <- "ForestGEO_spp_website.csv"
spp <- read.csv(sppFile,stringsAsFactors=FALSE)

## pre-cleaning?

## open database connection & grab current species list
dbparms.psql <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password='bety')
con <- db.open(dbparms.psql)
bety.species <- data.table(db.query("SELECT * FROM species", con))
bety.species.dt <- data.table(db.query("SELECT DISTINCT id as bety_species_id, scientificname as bety_species, species, genus FROM species", con))
bety.species.dt[, bety.species.lower := tolower(bety_species)]

# a. Get unique species list from ForestGEO

## loop over species
for(i in seq_len(nrow(spp))){  
  ## check to see if species exists in database
  
  ## insert record

  
  
}


db.close(con)
