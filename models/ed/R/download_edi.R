#' Download ED inputs
#'
#' Download and unzip common ED inputs from a public Open Science Framework 
#' (OSF) repository (https://osf.io/b6umf). Inputs include the Olson Global 
#' Ecosystems (OGE) database (`oge2OLD`) and the `chd` and `dgd` databases.
#'
#' The total download size around 28 MB.
#'
#' @param directory Target directory for unzipping files. Will be created if it 
#' doesn't exist.
#' @return `TRUE`, invisibly
#' @export
download_edi <- function(directory) {
  download_link <- "https://files.osf.io/v1/resources/b6umf/providers/osfstorage/5a948ea691b689000fa2a588/?zip="
  target_file <- paste0(directory, ".zip")
  download.file(download_link, target_file)
  unzip(target_file, exdir = directory)
  invisible(TRUE)
}
