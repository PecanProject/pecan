# 4. Match species between TRY and BETY
source("common.R")
load("try.3.RData")

# Get unique species list from BETY
# NOTE: This is set to PSQL-PEcAn because my clone of BETY was missing all the species
dbparms.psql <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password='bety', host='psql-pecan.bu.edu')
con.psql <- db.open(dbparms.psql)
bety.species.dt <- data.table(db.query("SELECT DISTINCT id as bety_species_id, scientificname as bety_species FROM species", con.psql))
bety.species.dt[, bety.species.lower := tolower(bety_species)]
db.close(con.psql)

# a. Get unique species list from TRY
try.species.unique <- try.dat[, unique(AccSpeciesName)]
try.species.dt <- data.table(try.species = try.species.unique,
                             try.species.lower = tolower(encodeString(try.species.unique)))
try.species.dt[, try.species.lower := gsub(" sp$", " spp.", try.species.lower)]


# Match TRY and BETY species.
## First a literal match (because it's fast in data.table)
setkey(try.species.dt, try.species.lower)
setkey(bety.species.dt, bety.species.lower)
match.species.dt <- bety.species.dt[try.species.dt]
try.unmatched <- match.species.dt[is.na(bety_species_id)]

## Second, a partial pattern match using grep
# TODO: Use taxize gnr_resolve() instead of grep for matching.
n.unmatched <- nrow(try.unmatched)
bety.index.match <- list()
message("Partial pattern match using grep...")
pb <- txtProgressBar(0, n.unmatched, style=3)
for(i in 1:n.unmatched){
  match.ind <- grep(sprintf(".*%s.*", try.unmatched[i,bety.species.lower]),
                    bety.species.dt[,bety_species], ignore.case=TRUE, perl=TRUE)
  if(length(match.ind) != 0){
    bety.index.match[[i]] <- match.ind
  }
  setTxtProgressBar(pb, i)
}

bety.index.nmatches <- sapply(bety.index.match, length)

# Unique matches -- can be added directly to the database
single.match <- which(bety.index.nmatches == 1)
single.match.inds <- unlist(bety.index.match[single.match])
setkey(match.species.dt, bety.species.lower)
match.species.dt[try.unmatched[single.match,bety.species.lower],
                 c("bety_species_id", "bety_species") := 
                   bety.species.dt[single.match.inds, list(bety_species_id,
                                                           bety.species.lower)]]

# Interactively sort out multiple matches
multiple.matches <- which(bety.index.nmatches > 1)
for(i in seq_along(multiple.matches)){
  m <- multiple.matches[i]
  try.sp <- try.unmatched[m, bety.species.lower]
  bety.sp <- bety.species.dt[bety.index.match[[m]], bety.species.lower]
  print(paste("TRY species:", try.sp))
  print(paste("BETY species:", paste(seq_along(bety.sp), bety.sp, collapse="; ", sep=" ")))
  user.choice <- readline("Select a species number (or enter 'n' for 'neither'):")
  if(user.choice == 'n'){
    next
  } else {
    user.choice <- as.numeric(user.choice)
    bety.index <- bety.index.match[[m]][user.choice]
  }
  match.species.dt[try.unmatched[bety.index, bety.species.lower],
                   c("bety_species_id", "bety_species") := 
                     bety.species.dt[bety.index, list(bety_species_id, bety.species.lower)]]
}

# b. Loop over TRY unmatched species list and add to BETY
try.unmatched.final <- match.species.dt[is.na(bety_species_id)]
sp.insert.query <- "INSERT INTO species(scientificname, notes) VALUES('%s', 'TRY_SPECIES') RETURNING id"
message("Looping over unmatched species and adding to BETY")
pb <- txtProgressBar(0, nrow(try.unmatched.final), style=3)
for(i in 1:nrow(try.unmatched.final)) {
  sp <- try.unmatched.final[i, encodeString(try.species)]
  sp <- fixquote(sp)
  sp.bety.id <- db.query(sprintf(sp.insert.query, sp), con)$id
  match.species.dt[try.species == sp, bety.species.id := as.character(sp.bety.id)]
  setTxtProgressBar(pb, i)
}


# c. Merge species ID into main TRY data.table
setkey(match.species.dt, try.species)
setkey(try.dat, AccSpeciesName)
try.dat <- match.species.dt[try.dat]
try.dat[, try.species.lower := NULL]
setnames(try.dat, "try.species", "AccSpeciesName")

save(try.dat, file = "try.4.RData", compress=TRUE)

# TODO: Grab species characteristics from TRY and add them here (or in another script)