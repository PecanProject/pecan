#* Function to be executed when /api/ping endpoint is called
#* If successful connection to API server is established, this function will return the "pong" message
#* @return Mapping containing response as "pong"
#* @author Tezan Sahu
ping <- function(req){
  res <- list(request="ping", response="pong")
  res
}

#* Function to get the status & basic information about the Database Host
#* @return Details about the database host
#* @author Tezan Sahu
status <- function(dbcon = global_db_pool) {
  
  ## helper function to obtain environment variables
  get_env_var = function (item, default = "unknown") {
    value = Sys.getenv(item)
    if (value == "") default else value
  }
  
  res <- list(host_details = PEcAn.DB::dbHostInfo(dbcon))
  res$host_details$authentication_required = get_env_var("AUTH_REQ")
  
  res$pecan_details <- list(
    version = get_env_var("PECAN_VERSION"), 
    branch = get_env_var("PECAN_GIT_BRANCH"), 
    gitsha1 = get_env_var("PECAN_GIT_CHECKSUM")
  )
  return(res)
}