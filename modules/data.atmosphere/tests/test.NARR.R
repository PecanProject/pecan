if(FALSE){   ##### NOT RUN
# Currently works for:
# NARR sites that haven't previously been extracted (need to clean up database)
# ED2 

rm(list = setdiff(ls(), lsf.str()))  # clear variables but not sourced functions
for (i in dbListConnections(PostgreSQL())) db.close(i) #close any stray database connections


site = 780 # Duplicate dbfiles are still a problem so for now need to choose sites that haven't been extracted yet
input = "NARR"
start_date = "1979-01-01 00:00:00"
end_date = "2013-12-31 23:59:00"
model = "ED2"
host = list(name = "geo.bu.edu")
bety = list(user = "bety", dbname = "bety", password="bety", host="psql-pecan.bu.edu")
dir ="/projectnb/dietzelab/pecan.data/input/"

outfolder <- met.process(site, input, start_date, end_date, model, host, bety, dir)

}
