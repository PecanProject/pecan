##' Test connection to database
##' 
##' Useful to only run tests that depend on database when a connection exists
##' @title db.exists
##' @return TRUE if database connection works; else FALSE
##' @export
##' @author David LeBauer
db.exists <- function(){
  tryCatch(query.base.con(), error = function(e)TRUE)
}