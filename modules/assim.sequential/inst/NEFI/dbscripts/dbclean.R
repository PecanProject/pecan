# Database scrubbing script.  Destroys all input and associated dbfile entries with NOAA_GEFS in the file name.
# @author Luke Dramko

library("dplyr")

args <- commandArgs(trailingOnly = TRUE)

len = length(args)
args_idx = 1

# Defaults
pattern <- "NOAA_GEFS"
delete <- FALSE

# Process command line arguments
if (len >= 3 && args[args_idx] == "-P") {
  args_idx = args_idx + 1
  pattern = args[args_idx]
  args_idx = args_idx + 1
}
if (len >= 1 && args[args_idx] == "TRUE") {
  delete <- TRUE
}

## Open Connection
bety <- dplyr::src_postgres(dbname   = 'bety', 
                            host     = 'psql-pecan.bu.edu', 
                            user     = 'bety', 
                            password = 'bety')

con <- bety$con
##List All tables
src_tbls(bety)

inputs = PEcAn.DB::db.query("SELECT * FROM inputs WHERE site_id=676", con = con)

print("-------------- All matching files ----------------")
print(inputs)
print("--------------------------------------------------")

inputs <- inputs[grepl(pattern, inputs$name),]
print("@@@---------- Files to be Deleted -----------@@@")
print(inputs)
print("@@@------------------------------------------@@@")

if (delete) {
  print("Moving on to delete files.")
} else {
  print("Quitting (default behavior).  Run with 'TRUE' as a command line argument to delete files.")
  quit("no")  ### Remove to run.  This is a safety feature.
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