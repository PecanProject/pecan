## met ensemble helper script

site <- list(id=1000000650)
input_met <- PalEON_ENS
start_date <- "01/01/1950"
end_date <- "12/31/2015"
model <- "SIPNET"
host = "localhost"
dbparms = list(user=bety,password = bety,host=localhost, dbname=bety,driver=PostgreSQL,write=TRUE)
dir = "/home/carya/outdir/dbfiles"
overwrite = FALSE