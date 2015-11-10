##' @name db.query.file.path
##' @title Get file path given id and machine
##' @param input_id
##' @param host_name
##' @param con : database connection
##' @export 
##' 
##' @author Betsy Cowdery 
query.file.path <- function(input.id, host_name, con){
  machine.host <- ifelse(host_name == "localhost",fqdn(),host_name)
  machine = db.query(paste0("SELECT * from machines where hostname = '",machine.host,"'"),con)
  dbfile = db.query(paste("SELECT file_name,file_path from dbfiles where container_id =",input.id," and container_type = 'Input' and machine_id =",machine$id),con)
  path <- file.path(dbfile$file_path,dbfile$file_name)
  if(file.exists(path)){
    return(path)
  }else{
    logger.error("Invalid file path")
  }
}
