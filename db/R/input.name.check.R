input.name.check <- function(inputname, con, dbparams){
  inputid <- db.query(paste0("SELECT id FROM inputs WHERE name = '", inputname, "'"), con, dbparams)[['id']]
}