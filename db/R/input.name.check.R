input.name.check <- function(inputname){
  
  inputid <- db.query(paste0("SELECT id FROM inputs WHERE name = '", inputname, "'"), con)[['id']]
  if (is.null(inputid)) {
    return(invisible(data.frame()))
  }
  invisible(dbfile.check('Input', inputid, con, hostname))
}