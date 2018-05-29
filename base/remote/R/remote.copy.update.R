#' Copy to remote and update DB
#' @param input_id Input ID, as a numeric or character
#' @param remote_dir remote folder path
#' @param remote_file_name remote file name, no need to provide if it's the same as local
#' @param host as in settings$host
#' @param con BETY database connection
#' @param stderr should stderr be returned
#' @return remote_id remote dbfile record
#'
#' @author Istem Fer
#' @export
remote.copy.update <- function(input_id, remote_dir, remote_file_name = NULL, host, con){

  PEcAn.remote::remote.execute.cmd(host, "mkdir", c("-p", remote_dir))

  local_file_record <- PEcAn.DB::db.query(paste("SELECT * from dbfiles where container_id =", input_id), con)

  if(is.null(remote_file_name)){
    local_file_name <- local_file_record$file_name
    if(length(local_file_name) > 1){
      PEcAn.logger::logger.warn(paste0("Multiple file names found in the DB and no remote file name provided. Using the first file name for remote file name: ",
                         local_file_record$file_name[1]))
      local_file_name <- local_file_record$file_name[1]
    }
    remote_file_name <- local_file_name
  }

  local_file_path  <- file.path(local_file_record$file_path, local_file_record$file_name)
  remote_file_path <- file.path(remote_dir, remote_file_name)

  remote.copy.to(host, local_file_path, remote_file_path)

  # update DB record
  remote_id <- PEcAn.DB::dbfile.insert(in.path = remote_dir, in.prefix = remote_file_name,
                type = local_file_record$container_type, id = local_file_record$container_id,
                con = con, hostname = host$name)


  return(remote_id)

} # remote.copy.update
