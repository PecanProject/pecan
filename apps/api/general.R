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
status <- function() {
  dbcon <- PEcAn.DB::betyConnect()
  res <- list(host_details = PEcAn.DB::dbHostInfo(dbcon))
  
  # Needs to be completed using env var
  res$pecan_details <- list(version="1.7.0", branch="api_1", gitsha1="unknown")
  return(res)
}