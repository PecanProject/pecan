library(testthat)

library(PEcAn.DB)
library(RPostgreSQL)
dbparms <- get_postgres_envvars(
	host = "localhost",
	driver = "PostgreSQL",
	user = "bety",
	dbname = "bety",
	password = "bety")

if(db.exists(dbparms)){
  con <- db.open(dbparms)
  PEcAn.logger::logger.setQuitOnSevere(FALSE)
  test_check("PEcAn.DB")
  db.close(con)
}
