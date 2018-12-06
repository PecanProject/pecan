# Removes all references of the given date from the database

library("dplyr")

args <- commandArgs(trailingOnly = TRUE)

len = length(args)
args_idx = 1

# Defaults
pattern <- "NOAA_GEFS"
delete <- FALSE

if (len > 0) {
  start_date <- args[1]
} else {
  print("Please enter a date to search for.")
  quit("no")
}

if (len > 1 && args[2] == "TRUE") {
  delete <- TRUE
}

## Open Connection
bety <- dplyr::src_postgres(dbname   = 'bety', 
                            host     = 'psql-pecan.bu.edu', 
                            user     = 'bety', 
                            password = 'bety')

con <- bety$con
##List All tables
# src_tbls(bety)

inputs = PEcAn.DB::db.query(paste0("SELECT * FROM inputs WHERE site_id=676 AND start_date='", start_date,
                                   "' AND name='NOAA_GEFS_SIPNET_site_0-676'"), con = con)
inputs = rbind(inputs, PEcAn.DB::db.query(paste0("SELECT * FROM inputs WHERE site_id=676 AND start_date='",start_date,
                                                 "' AND name LIKE 'NOAA_GEFS__'"), con))

inputs = rbind(inputs, PEcAn.DB::db.query(paste0("SELECT * FROM inputs WHERE site_id=676 AND start_date='",start_date,
                                                 "' AND name LIKE 'NOAA_GEFS___'"), con))

print("-------------- All matching files ----------------")
print(inputs)
print("--------------------------------------------------")

inputs <- inputs[grepl(pattern, inputs$name),]
print("@@@---------- Files to be Deleted -----------@@@")
print(inputs)
print("@@@------------------------------------------@@@")

if (!delete) {
  print("Run with TRUE as the second command line argument to delete files.")
  quit("no")
}

for (i in 1:nrow(inputs)) {
  print(paste0("i = ", i))
  print(inputs[i,])
  print("id")
  print(inputs[i,]$id)
  
  hostname = PEcAn.remote::fqdn()
  print(paste0("hostname = ", hostname))
  
  dbfile <- PEcAn.DB::dbfile.check(type = 'Input', container.id = inputs[i,]$id, con = con, hostname = hostname, machine.check = TRUE)
  
  print("dbfile")
  print(dbfile)
  
  if (!is.null(dbfile$id)) {
    PEcAn.DB::db.query(query =paste0("DELETE FROM dbfiles where id =", dbfile$id),
                       con)
    
    print(paste0("dbfile ", dbfile$id ," removed."))
  }
  
  PEcAn.DB::db.query(query =paste0("DELETE FROM inputs where id =", inputs[i,]$id),
                     con)
  print(paste0("inputfile ", inputs[i,]$id ," removed."))
}

PEcAn.DB::db.close(con)