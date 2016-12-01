## PFT Builder
##
## Goal is to facilitate bulk associations between species and PFTs
## Especially for high-diversity systems, such as the tropics

PFT_name <- "broadleaf_evergreen_tropical_tree"

## Get PFT id
dbparms.psql <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password='bety',host='128.197.230.32')
con <- db.open(dbparms.psql)
PFT <- data.table(db.query(paste0("SELECT * from pfts where name = '",PFT_name,"'"), con))

## GET PFT->species
PFT_species <- data.table(db.query(paste0("SELECT * from pfts_species where pft_id = '",PFT$id,"'"), con))

## Get new species list
species <- data.table(db.query("SELECT * from species where notes LIKE '%tropical%'", con))
## note: this won't match all the species that were ALREADY in BETY when ForestGEO was added
sppFile <- "ForestGEO_spp_website.csv"
spp <- read.csv(sppFile,stringsAsFactors=FALSE,strip.white = TRUE)
spp <- spp[-duplicated(spp$Species),]
species <- data.table(db.query("SELECT * from species", con))
sel <- which(tolower(species$scientificname) %in% tolower(spp$Species))
species <- species[sel,]

## Associate species with PFT
for(i in seq_len(nrow(species))){
  
  if( species$id[i] %in% PFT_species$specie_id){
    print(c(i,'ALREADY EXITS'))
  } else {
    print(i)
    ## insert record
    query <- paste0("INSERT INTO pfts_species (pft_id, specie_id) SELECT ",
                    PFT$id,", ",
                    species$id[i],
                    " WHERE NOT EXISTS ( ",
                    "SELECT pft_id, specie_id FROM pfts_species where pft_id = ",PFT$id,
                    " and specie_id = ", species$id[i],
                    " );")
    db.query(query, con)
    
 
  }
}
db.close(con)
