library(dplyr)

#* Obtain the encrypted password for a user
#* @param username Username, which is also the 'salt'
#* @param password Unencrypted password
#* @param secretkey Secret Key, which if NA, is set to 'notasecret'
#* @return Encrypted password
#* @author Tezan Sahu
get_crypt_pass <- function(username, password, secretkey = NA) {
  secretkey <- if(is.na(secretkey)) "notasecret" else secretkey
  dig <- secretkey
  salt <- username
  for (i in 1:10) {
    dig <- digest::digest(
      paste(dig, salt, password, secretkey, sep="--"), 
      algo="sha1", 
      serialize=FALSE
    )
  }
  return(dig)
}



#* Check if the encrypted password for the user is valid
#* @param username Username
#* @param crypt_pass Encrypted password
#* @return TRUE if encrypted password is correct, else FALSE
#* @author Tezan Sahu
validate_crypt_pass <- function(username, crypt_pass) {

  res <- tbl(global_db_pool, "users") %>%
    filter(login == username,
           crypted_password == crypt_pass) %>%
    collect()

  if (nrow(res) == 1) {
    return(res$id)
  }
  
  return(NA)
}

#* Check if the API key exists
#* @param api_key API Key
#* @return user details if apikey exists, else Invalid API Key
#* @author Nihar Sanda
validate_api_key <- function(api_key) {
  res <- tbl(global_db_pool, "users") %>%
    filter(apikey == api_key) %>%
    collect()
  
  if (nrow(res) == 1) {
    return(res)
  }
  
  return(NA) 
}

#* Filter to authenticate a user calling the PEcAn API
#* @param req The request
#* @param res The response to be set
#* @return Appropriate response
#* @author Tezan Sahu
authenticate_user <- function(req, res) {
  # Fix CORS issues
  res$setHeader("Access-Control-Allow-Origin", "*")
  
  # If the API endpoint that do not require authentication
  if (
    Sys.getenv("AUTH_REQ") == FALSE ||
    grepl("swagger", req$PATH_INFO, ignore.case = TRUE) || 
    grepl("openapi.json", req$PATH_INFO, fixed = TRUE) ||
    grepl("/api/ping", req$PATH_INFO, ignore.case = TRUE) ||
    grepl("/api/status", req$PATH_INFO, ignore.case = TRUE))
  {
    req$user$userid <- NA
    req$user$username <- ""
    return(plumber::forward())
  }
  
  if (!is.null(req$HTTP_AUTHORIZATION)) {
    # HTTP_AUTHORIZATION is of the form "Basic <base64-encoded-string>", 
    # where the <base64-encoded-string> is contains <username>:<password>
    auth_details <- strsplit(rawToChar(jsonlite::base64_dec(strsplit(req$HTTP_AUTHORIZATION, " +")[[1]][2])), ":")[[1]]
    username <- auth_details[1]
    password <- auth_details[2]
    crypt_pass <- get_crypt_pass(username, password)
    
    userid <- validate_crypt_pass(username, crypt_pass)
    
    if(! is.na(userid)){
      req$user$userid <- userid
      req$user$username <- username
      return(plumber::forward())
    }
    
  }
  
  if(!is.null(req$HTTP_X_API_KEY)) {
    key <- req$HTTP_X_API_KEY
    # HTTP_X_API_KEY is of the form "api_key"
    user <- validate_api_key(key)
    userid <- user$id
    username <- user$login
    print(userid)
    print(username)
      
    if(! is.na(userid)){
      req$user$userid <- userid
      req$user$username <- username
      return(plumber::forward())
    }
  }
  
  res$status <- 401 # Unauthorized
  return(list(error="Authentication required"))
}
