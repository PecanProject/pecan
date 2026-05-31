#' Load and standardize arbitrary tabular data using a YAML mapping configuration
#'
#' @param data.path character, file path to the tabular data (e.g. .csv)
#' @param mapping.path character, file path to the YAML mapping configuration
#' @return A standardized data frame with column names mapped to PEcAn standard vocabulary
#' @export
#' @importFrom yaml read_yaml
#' @importFrom dplyr rename
load_and_map_data <- function(data.path, mapping.path) {
  # Load the raw data (currently assuming CSV, but could be extended to NetCDF)
  dat <- utils::read.csv(data.path, as.is = TRUE, check.names = FALSE)
  
  # Load the YAML mapping
  # The YAML should look like:
  # variables:
  #   airT: TA_F
  #   NEE: NEE_PI
  mapping <- yaml::read_yaml(mapping.path)
  
  if (is.null(mapping$variables)) {
    stop("YAML mapping must contain a 'variables' section.")
  }
  
  # Create a named vector for dplyr::rename (new_name = old_name)
  rename_vector <- unlist(mapping$variables)
  
  # Only rename columns that exist in the raw data
  valid_renames <- rename_vector[rename_vector %in% colnames(dat)]
  
  # Apply renaming
  if (length(valid_renames) > 0) {
    # dplyr::rename syntax expects: rename(df, new_name = old_name)
    # Using tidy evaluation with !!!
    dat <- dplyr::rename(dat, !!!valid_renames)
  } else {
    warning("No matching columns found in the dataset to map.")
  }
  
  return(dat)
}
