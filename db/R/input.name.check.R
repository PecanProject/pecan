input.name.check <- function(inputname, con){
  inputid <- db.query(paste0("SELECT id FROM inputs WHERE name = '", inputname, "'"), con)[['id']]
}