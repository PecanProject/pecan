# 5. Add TRY citations into BETY
source("common.R")
load("try.4.RData")
library(rcrossref)
library(stringr)

# a. Get unique citation list from TRY.
refs <- try.dat[, .N, by=Reference][,N := NULL]
refs[, bety.citation.id := character(nrow(refs))]

# b. Get data for each citation via rcrossref (author, year, title, journal, doi)
check.query <- "SELECT id FROM citations WHERE doi LIKE '%s'"
insert.query <- "INSERT INTO citations(author, year, title, journal, doi) VALUES('%s', %d, '%s', '%s', '%s') RETURNING id"
message("Looping over citations and adding to BETY")
pb <- txtProgressBar(0, nrow(refs), style=3)
for(i in 1:nrow(refs)){
  r <- refs[i, encodeString(Reference)]
  if(nchar(r) < 20){
    # Unpublished data -- skip.
    # refs[i, DOI := "unpublished"]
    next
  } else {
    result <- cr_works(query = r)$data[1,]
    doi <- fixquote(result$DOI)
    # i. Check if DOI already in BETY.
    check.df <- db.query(sprintf(check.query, doi), con)
    if(nrow(check.df) == 0){
      # ii. If no, add.
      author <- fixquote(paste0(result$author[[1]][1,], collapse=", "))
      year <- year(result$deposited)
      title <- fixquote(result$title)
      journal <- fixquote(result$container.title)
      check.df <- db.query(sprintf(insert.query, author, year, title, journal, doi), con)
    }
    # Add BETY citation ID to try.dat
    bety_id <- check.df$id[1]
    refs[i, bety.citation.id := bety_id]
  }
  setTxtProgressBar(pb, i)
}

# d. Merge citation_id back into base TRY data.
setkey(refs, Reference)
setkey(try.dat, Reference)
try.dat <- refs[try.dat]

save(try.dat, file="try.5.RData", compress=TRUE)
