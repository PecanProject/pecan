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
spp <- read.csv(sppFile,stringsAsFactors=FALSE,strip.white = TRUE)

## pre-cleaning?
spp <- spp[-duplicated(spp$Species),]

## open database connection & grab current species list
dbparms.psql <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password='bety')
con <- db.open(dbparms.psql)
bety.species <- data.table(db.query("SELECT * FROM species", con))
bety.species.dt <- data.table(db.query("SELECT DISTINCT id as bety_species_id, scientificname as bety_species, species, genus FROM species", con))
bety.species.dt[, bety.species.lower := tolower(bety_species)]

base.note <- "ForestGEO Neotropical tree http://ctfs.si.edu/webatlas/neotropicaltree/"

## loop over species
for(i in seq_len(nrow(spp))){  
  
  ## check to see if genus and species exists in database
  if(!(tolower(spp$Species[i]) %in% bety.species.dt$bety.species.lower)){
    print(i)
    ## generate binomial
    binom <- strsplit(spp$Species[i]," ",fixed=TRUE)[[1]]
    genus <- binom[1]
    species <- paste(binom[-1],collapse = " ")
    authority <- stringi::stri_trans_general(spp$Authority[i], "latin-ascii")
    authority <- sub("'","`",authority)
    note <- paste(base.note,"\nTaxanomic Authority:",authority)
    ## insert record
    query <- paste0("INSERT INTO species (genus, species, scientificname, \"Family\", \"GrowthForm\", notes) SELECT '",
                   paste(
                   genus,
                   species,
                   spp$Species[i],
                   spp$Family[i],
                   spp$Growth.Form[i],
                   note,sep = "','"),
                   "' WHERE NOT EXISTS ( ",
                     "SELECT scientificname FROM species where scientificname = '", 
                    spp$Species[i],"'",
                   " );")
            ## Note: the WHERE NOT EXISTS clause provides additional protection against duplication
    db.query(query, con)

  } else {
    print(c(i,'ALREADY EXITS',spp$Species[i]))
  }
  
}


db.close(con)
