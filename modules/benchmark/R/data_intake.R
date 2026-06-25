#' Load and standardize arbitrary tabular data using a YAML mapping configuration
#'
#' @param data.path character, file path to the tabular data (e.g. .csv)
#' @param mapping.path character, file path to the YAML mapping configuration
#' @return A standardized data frame with column names mapped to PEcAn standard vocabulary
#' @export
#' @importFrom yaml read_yaml
#' @importFrom dplyr rename
load_and_map_data <- function(data.path, mapping.path) {
  # Load the YAML mapping first to know which variables we need
  # The YAML should look like:
  # variables:
  #   airT: TA_F
  #   NEE: NEE_PI
  mapping <- yaml::read_yaml(mapping.path)
  
  if (is.null(mapping$variables)) {
    PEcAn.logger::logger.severe("YAML mapping must contain a 'variables' section.")
  }
  
  # Create a named vector for dplyr::rename (new_name = old_name)
  rename_vector <- unlist(mapping$variables)
  required_vars <- unname(rename_vector)
  
  # Load the raw data based on file extension
  if (grepl("\\.nc$", data.path, ignore.case = TRUE)) {
    # If NetCDF, we only load the variables requested in the YAML mapping
    # Assuming standard NA string representations for now, can be expanded via YAML
    dat <- load_x_netcdf(data.path, format = list(na.strings = c("-9999", "-9999.0", "NA")), site = NULL, vars = required_vars)
  } else {
    # Default to CSV
    dat <- utils::read.csv(data.path, as.is = TRUE, check.names = FALSE)
  }
  
  # Only rename columns that exist in the raw data
  valid_renames <- rename_vector[rename_vector %in% colnames(dat)]
  
  # Apply renaming
  if (length(valid_renames) > 0) {
    # dplyr::rename syntax expects: rename(df, new_name = old_name)
    # Using tidy evaluation with !!!
    dat <- dplyr::rename(dat, !!!valid_renames)
  } else {
    PEcAn.logger::logger.warn("No matching columns found in the dataset to map.")
  }
  
  return(dat)
}
