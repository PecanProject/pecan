#* Function to be executed when /api/ping endpoint is called
#*
#* If successful connection to API server is established, this function will return the "pong" message
#* @return Mapping containing response as "pong"
#* @author Tezan Sahu
ping <- function(){
  res <- list(request="ping", response="pong")
  res
}

