#' Copy to remote and update DB
#' @param input_id Input ID, as a numeric or character
#' @param remote_dir remote folder path
#' @param local_file_path full path name to local file that needs to be copied, e.g. "/fs/data1/dbfiles/.../.../*.css"
#' @param remote_file_name remote file name, no need to provide if it's the same as local
#' @param host as in settings$host
#' @param con BETY database connection
#' @param stderr should stderr be returned
#' @return remote_id remote dbfile record
#'
#' @author Istem Fer
#' @export
remote.copy.update <- function(input_id, remote_dir, local_file_path, remote_file_name = NULL, host, con){

  PEcAn.remote::remote.execute.cmd(host, "mkdir", c("-p", remote_dir))

  if(is.null(remote_file_name)){
    remote_file_name <- basename(local_file_path)
  }

  remote_file_path <- file.path(remote_dir, remote_file_name)

  remote.copy.to(host, local_file_path, remote_file_path)

  type <- db.query(paste("SELECT container_type from dbfiles where container_id =", putveg.id[[i]]), con)
  
  # update DB record
  remote_id <- PEcAn.DB::dbfile.insert(in.path = remote_dir, in.prefix = remote_file_name,
                type = unique(type), id = input_id$input.id,
                con = con, hostname = host$name)


  return(remote_id)

} # remote.copy.update
