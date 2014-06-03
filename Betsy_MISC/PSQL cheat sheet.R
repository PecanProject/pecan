# Useful PosgreSQL things

library(PEcAn.all)
library(RPostgreSQL)

dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
con     <- db.open(dbparms)

input.id  <- 288
outfolder_test <- "/projectnb/cheas/pecan.data/input/NARR_CF_test/"
outfolder      <- "/projectnb/cheas/pecan.data/input/NARR_CF/"

# Update name

db.query(paste("UPDATE inputs SET name = 'NARR_CF' where id =",input.id),con)


# Get input
input_test = db.query(paste("SELECT * from inputs where id =",NARR_extract.id),con)

# Get db file 
dbfile_x = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input'"),con)


# Check if files is in dbfiles
file = db.query(paste0("SELECT * from dbfiles where file_path = '", outfolder, "'"),con)
if(all(dim(file) == c(0, 0))){
  db.query(paste0("INSERT INTO dbfiles (file_path, file_name) VALUES ('", outfolder, "','NARR.')"),con)
  db.query(paste0("UPDATE dbfiles SET container_id = ",input.id,", container_type = 'Input' where file_path = '",outfolder,"'"),con)
}else{
  if(!(file$container_id == input.id && file$container_type == 'Input')){
    db.query(paste0("UPDATE dbfiles SET container_id = ",input.id,", container_type = 'Input' where file_path = '",outfolder,"'"),con)
  }else{print("All set up!")}
}
file = db.query(paste0("SELECT * from dbfiles where file_path = '", outfolder, "'"),con)
file_test = db.query(paste0("SELECT * from dbfiles where file_path = '", outfolder_test, "'"),con)

db.query(paste0("DELETE from dbfiles where file_name = '", "NARR_CF_site_1161", "'"),con)

y <- db.query(paste0("DELETE from dbfiles where file_path = '", outfolder, "'"),con)
db.query(paste0("DELETE FROM inputs WHERE name = '", in.name, "'"), con, dbparams)

for (i in dbListConnections(PostgreSQL())) db.close(i)

x <- db.query(paste0("SELECT * FROM dbfiles WHERE id=", 1000000107), con)

now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")


recent_input <- db.query(paste("SELECT * FROM inputs ORDER BY created_at DESC LIMIT 10"),con)
recent_dbfile <- db.query(paste("SELECT * FROM dbfiles ORDER BY created_at DESC LIMIT 400"),con)

recent_dbfile <- db.query(paste0("SELECT * FROM dbfiles WHERE created_at LIKE '","2014-06-02%","'"),con)
