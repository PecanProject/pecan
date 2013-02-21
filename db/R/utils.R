##' Test connection to database
##' 
##' Useful to only run tests that depend on database when a connection exists
##' @title db.exists
##' @return TRUE if database connection works; else FALSE
##' @export
##' @author David LeBauer
db.exists <- function(...){
  if(!exists("settings")){
    settings <- list(database = 
                       list(userid = "bety", 
                            passwd = "bety", 
                            host = "localhost",
                            name = "bety"))
  }
  ans <- tryl(query.base.con(settings))
  return(ans)
}
