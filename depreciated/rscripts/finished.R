# ----------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------
library(XML)
require(PECAn)

settings.file <- Sys.getenv('PECANSETTINGS')
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

# Update runs table
con <- query.bety.con(dbname   = settings$database$name,
                      password = settings$database$passwd,
                      username = settings$database$userid,
                      host     = settings$database$host)
dbSendQuery(con, paste("UPDATE runs SET finished_at =  NOW() WHERE id = ", settings$run$id))
