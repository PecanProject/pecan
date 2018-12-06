# Creates a database connection to BETY.  Useful for working in the console.

dbparms = list()
dbparms$dbname = "bety"
dbparms$host = "128.197.168.114"
dbparms$user = "bety"
dbparms$password = "bety"


#--------------------------------------------------
#Connection code copied and pasted from met.process
bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                            host     = dbparms$host, 
                            user     = dbparms$user, 
                            password = dbparms$password)

con <- bety$con #Connection to the database.  dplyr returns a list.
