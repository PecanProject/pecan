#' Read a PFT file (RDS, RData, or CSV)
#'
#' This utility reads a PFT related file, automatically handling RDS, RData, or CSV formats.
#'
#' @importFrom utils read.csv
#' @param file_path Full path to the file to read
#' @return The object read from the file
#' @export
read_pft_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste(file_path, " does not exist"))
  }

  ext <- tolower(tools::file_ext(file_path))

  # Choose the reader
  strategy <- switch(ext,
                     rds   = readRDS,
                     RData = PEcAn.utils::load_local,
                     csv   = read.csv,
                     stop("Unsupported file type: ", ext))

  # Read the data
  data <- strategy(file_path)

  if (ext == "RData" && is.list(data) && length(data) == 1) {
    data <- data[[1]]
  }

  return(data)
}
