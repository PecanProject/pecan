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
  
  # replace all non-alphanumeric characters with "_"
  basename <- base::gsub("[^a-zA-Z0-9 :]", "_", format_name)
  
  ## Convert site_id into shorter format
  long_id <- as.numeric(site_id) # convert scientific notation to numeric
  new_id <- paste0(long_id %/% 1e+09, "-", long_id %% 1e+09) 
  
  # Combine
  autoDirName <- paste(basename, "site", new_id, sep = "_")
  return(autoDirName)
}

