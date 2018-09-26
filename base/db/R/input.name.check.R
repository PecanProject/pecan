input.name.check <- function(inputname, con){
  db.query(query = paste0("SELECT id FROM inputs WHERE name = '", inputname, "'"), con = con)[['id']]
}
