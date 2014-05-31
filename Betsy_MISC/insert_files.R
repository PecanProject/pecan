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
input = db.query(paste("SELECT * from inputs where id =",input.id),con)

# Get db file 
dbfile = db.query(paste("SELECT * from dbfiles where container_id =",input.id," and container_type = 'Input'"),con)


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

db.query(paste0("DELETE from dbfiles where file_path = '", outfolder, "'"),con)

