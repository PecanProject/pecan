# 4. Match species between TRY and BETY
source("common.R")
load("try.3.RData")

user_id <- "1000000013"     # Alexey Shiklomanov
dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password='bety', host='psql-pecan.bu.edu')
con <- db.open(dbparms)

# a. Get unique species list from TRY
try.species.unique <- try.dat[, unique(AccSpeciesName)]
try.species.dt <- data.table(try.species = try.species.unique)
try.species.dt[, try.species.lower := tolower(encodeString(try.species))]

# Get unique species list from BETY
bety.species.dt <- data.table(db.query("SELECT DISTINCT id as bety_species_id, scientificname as bety_species FROM species", con))
bety.species.dt[, bety.species.lower := tolower(bety_species)]

# Match TRY and BETY species.
## First a literal match
setkey(try.species.dt, try.species.lower)
setkey(bety.species.dt, bety.species.lower)
match.species.dt <- bety.species.dt[try.species.dt]
try.unmatched <- match.species.dt[is.na(bety_species_id)]

## Next, a fuzzy match
# match.adist <- adist(try.unmatched$bety.species.lower[1:5],
#                      bety.species.dt$bety.species.lower)
library(stringdist)
for(i in 1:nrow(try.unmatched)){
  match.strdist.full <- stringdist(try.unmatched$bety.species.lower[i], 
                                   bety.species.dt$bety.species.lower, 
                                   method = 'dl')
  min.dist <- min(match.strdist.full)
  min.index <- which.min(match.strdist.full)
  print(sprintf("try: %s, bety: %s, distance: %f",
                try.unmatched$bety.species.lower[i],
                bety.species.dt$bety.species.lower[min.index], min.dist))
  readline("Enter:")
}


# b. Loop over TRY species list, and add BETY species ID to TRY data.table.
sp.query <- "SELECT id, scientificname FROM species WHERE scientificname ~ '.*%s.*'"
pb <- txtProgressBar(0, nrow(try.species.dt), style=3)
for(i in 1:nrow(try.species.dt)){
  sp <- try.species.dt[i, try.species]
  match.df <- try(db.query(sprintf(sp.query, sp), con))
  if(nrow(match.df) == 0){
    warning(sprintf("No match for species '%s'. Skipping", sp))
    next
  } else {
    try.species.dt[i, bety.species.id := as.character(match.df$id[1])]
  }
  setTxtProgressBar(pb, i)
}
  
}
#   ii. Try a fuzzy merge (SELECT id,scientificname FROM species WHERE scientificname LIKE AccSpeciesName)
#   iii. Insert the species?
# c. Merge species ID into main TRY data.table