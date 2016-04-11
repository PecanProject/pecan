# 6. Add TRY data to BETY
source("common.R")
load("try.5.RData")

setkey(try.dat, ObservationID)
try.entities <- try.dat[, .GRP, by=ObservationID]
try.entities[, c("bety.entity.id", "bety.trait.id") := character(nrow(try.entities))]

# a. Loop over entities...
add.entity.query <- "INSERT INTO entities(name, notes) VALUES('%s', '%s') RETURNING id"
insert.trait <- function(vals){
  vals.na <- which(is.na(vals))
  vals.sub <- vals[-vals.na]
  n <- names(vals.sub)
  lhs <- paste(n, collapse = ",")
  rhs <- paste(vals.sub, collapse=",")
  query.string <- sprintf("INSERT INTO traits(%s) VALUES(%s) RETURNING id", lhs, rhs)
  id <- db.query(query.string, con)$id
  return(id)
}

string <- function(x) {
  x <- fixquote(x)
  return(sprintf("'%s'", x))
}

message("Looping over entities and adding values to BETY")
pb <- txtProgressBar(0, nrow(try.entities), style=3)
for(i in 1:nrow(try.entities)){
  # i. Add entity to entities table
  entity <- try.entities[i, ObservationID]
  entity.name <- fixquote(paste0("TRY_OBSERVATION_", entity))
  check <- db.query(sprintf("SELECT id FROM entities WHERE name LIKE '%s'", entity.name), con)
  if(length(check) > 0) next
  try.sub <- try.dat[ObservationID == entity]
  entity.notes <- fixquote(try.sub[, paste(unique(DatasetID),
                                           unique(Dataset),
                                           unique(ObservationID),
                                           collapse = " ; ")])
  entity.id <- db.query(sprintf(add.entity.query, entity.name, entity.notes), con)$id
  # ii. Store entity_id.
  try.entities[, bety.entity.id := entity.id]
  # iii. Loop over rows...
  for(j in 1:nrow(try.sub)){
    vals <- list(
      site_id = try.sub[j, bety.site.id],
      specie_id = try.sub[j, bety.species.id],
      citation_id = try.sub[j, bety.citation.id],
      mean = try.sub[j, StdValue],
      n = try.sub[j, Replicates],
      user_id = user_id,
      entity_id = entity,
      variable_id = try.sub[j, bety_id],
      notes = string(try.sub[j, paste("TRY_VALUE", DatasetID, ObservationID, DataID, ObsDataID)])
    )
    id <- insert.trait(vals)
    try.entities[i, bety.trait.id := paste(bety.trait.id, id)]
  }
  setTxtProgressBar(pb, i)
}

save(try.entities, file="try.entities.RData")