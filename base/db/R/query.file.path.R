##' Get file path given id and machine
##'
##' @param host_name character: machine where the file lives
##' @param con : database connection
##' @param input.id database id of the file ("container") to find
##' @export query.file.path
##'
##' @author Betsy Cowdery
query.file.path <- function(input.id, host_name, con){
  machine.host <- PEcAn.DB::default_hostname(host_name)
  machine <- db.query(query = paste0("SELECT * from machines where hostname = '",machine.host,"';"), con = con)
  dbfile <- db.query(
    query = paste(
      "SELECT file_name,file_path from dbfiles where container_id =", input.id,
      " and container_type = 'Input' and machine_id =", machine$id, ";"
    ),
    con = con
  )
  path <- file.path(dbfile$file_path,dbfile$file_name)
  cmd <- paste0("file.exists( '",path,"')")
  PEcAn.remote::remote.execute.R(script = cmd, host = machine.host, verbose=TRUE)
  #   Check - to be determined later
  #   if(file.exists(path)){
  #     return(path)
  #   }else{
  #     logger.error("Invalid file path")
  #   }
  return(path)
}

