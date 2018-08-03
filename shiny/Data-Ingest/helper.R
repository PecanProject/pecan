# This helper file stores all the relevant functions to be called in the shiny app
#' Auto-create directory name for local file upload
#'
#' @param format_name Format name in type: character string
#' @param site_id Integer, scientific notation, or character string that will be converted to an integer and will be the trailing identifier for the firectory name
#'
#' @return Self-generated directory name to store files uploaded via local upload. 
#' @export
#' @author Liam Burke (liam.burke24@gmail.com)
#'
#' @examples
#' auto.name.directory(format_name = "LTER-hf-103", site_id = 1000004955)
auto.name.directory <- function(format_name, site_id){
  
  # Split the format_name by the first non-alphanumeric character
  splits <- base::strsplit("LTER-hf-103", "[^a-zA-Z0-9 :]")
  
  # Grab first split
  basename <- splits[[1]][1]
  
  ## Convert site_id into shorter format
  long_id <- as.numeric(site_id) # convert scientific notation to numeric
  new_id <- base::sub('(?<=.{1}).+0', "-", long_id, perl = TRUE) # Replace all 0's with "-"
  
  # Combine
  autoDirName <- paste(basename, "site", new_id, sep = "_")
  return(autoDirName)
}

