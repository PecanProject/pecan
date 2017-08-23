## Code to insert ForestGEO species list
## Mike Dietze
## some of the code here is borrowed from Alexey's import-try 04.match.species
library(data.table)
library(PEcAn.DB)
library(RPostgreSQL)

## function from chartr example
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
to.leet <- function(x){
  x <- gsub("0","o",x)
  x <- gsub("1","i",x)
  x <- gsub("2","z",x)
  x <- gsub("3","e",x)
  x <- gsub("4","a",x)
  x <- gsub("5","s",x)
  x <- gsub("6","b",x)
  x <- gsub("7","l",x)
  x <- gsub("8","q",x)
  x <- gsub("9","g",x)
  return(x)  
}

## Load raw data
## File generated 11/14/16 by cut-and-paste from web query
## http://ctfs.si.edu/webatlas/neotropicaltree/
## then saved as CSV. CTFS needs a better API
sppFile <- "ForestGEO_spp_website.csv"
spp <- read.csv(sppFile,stringsAsFactors=FALSE,strip.white = TRUE)

## pre-cleaning?
spp <- spp[-duplicated(spp$Species),]

## open database connection & grab current species list
dbparms.psql <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password='bety',host='128.197.230.32')
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
    spp$Species[i] <- gsub("'","",spp$Species[i],fixed = TRUE)
    binom <- strsplit(spp$Species[i]," ",fixed=TRUE)[[1]]
    genus <- binom[1]
    genus <- gsub("(","",genus,fixed=TRUE)
    genus <- gsub(")","",genus,fixed=TRUE)
    genus <- gsub("/","-",genus,fixed=TRUE)
    genus <- gsub(":","-",genus,fixed=TRUE)
    genus <- gsub(".","",genus,fixed=TRUE)
    genus <- sub("_","-",genus,fixed = TRUE)
    genus <- gsub("_","",genus,fixed = TRUE) ## drop later occurrances
    genus <- sub("?","UNK",genus,fixed=TRUE) ## switch on ? to UNK
    genus <- gsub("?","",genus,fixed = TRUE) ## drop any later ?
    genus <- to.leet(genus)
    genus <- capwords(genus,strict=TRUE)
    cross <- which(tolower(binom)=="x")
    if(length(cross)>0){
      binom <- binom[-cross]  ## remove cross from name because violates constraint
    }
    species <- paste(binom[-1],collapse = " ")
    species <- gsub("(","",species,fixed=TRUE)
    species <- gsub(")","",species,fixed=TRUE)
    species <- gsub("[","",species,fixed=TRUE)
    species <- gsub("]","",species,fixed=TRUE)
    species <- gsub(".","",species,fixed=TRUE)
    species <- gsub("&","",species,fixed=TRUE)
    species <- gsub("=","",species,fixed=TRUE)
    species <- gsub("/","-",species,fixed=TRUE)
    species <- gsub(":","-",species,fixed=TRUE)
    species <- gsub("#","-",species,fixed=TRUE)
    species <- gsub(",","-",species,fixed=TRUE)
    species <- sub("_","-",species,fixed = TRUE)
    species <- gsub("_","",species,fixed = TRUE) ## drop later occurrances
    species <- sub("?","UNK",species,fixed=TRUE) ## switch on ? to UNK
    species <- gsub("?","",species,fixed = TRUE) ## drop any later ?
    species <- to.leet(species)
    if(nchar(species) == 1) species <- paste0(species,species) ## if length 1, duplicated (shouldn't have to do this)
    first <- strsplit(species,"-")[[1]][1]  ## also check length of part before hyphen. Again, shouldn't be required
    if(!is.na(first) & nchar(first) == 1){
      species <- paste0(first,species)
    }
    second <- strsplit(species,"-")[[1]][2]  ## also check length of part after hyphen. Again, shouldn't be required
    if(!is.na(second) & nchar(second) == 1){
      species <- paste0(species,second)
    }
    authority <- stringi::stri_trans_general(spp$Authority[i], "latin-ascii")
    authority <- gsub("'","`",authority)
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
