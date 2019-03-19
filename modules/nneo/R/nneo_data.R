#' Data
#'
#' @export
#' @param product_code (character) a product code. required.
#' @param site_code (character) a site code. required.
#' @param year_month (character) YYYY-MM month to check for files. required.
#' @param package (character) Package type to return, basic or expanded.
#' optional.
#' @param filename (character) a file name. optional.
#' @template curl
#' @return `nneo_data` returns a list, while `nneo_file` returns
#' a tibble/data.frame
#'
#' @details `nneo_data` gets files available for a given
#' product/site/month combination.
#'
#' `nneo_file` gets a file, and returns a data.frame
#'
#' @examples \dontrun{
#' nneo_data(product_code = "DP1.00098.001", site_code = "HEAL",
#'   year_month = "2016-05")
#'
#' nneo_data(product_code = "DP1.00098.001", site_code = "HEAL",
#'   year_month = "2016-05")
#'
#' ## with a package
#' nneo_data(product_code = "DP1.00098.001", site_code = "HEAL",
#'   year_month = "2016-05", package = "basic")
#' nneo_data(product_code = "DP1.00098.001", site_code = "HEAL",
#'   year_month = "2016-05", package = "expanded")
#'
#' ## with a file name
#' fname <- "NEON.D19.HEAL.DP1.00098.001.00000.000.040.001.RH_1min.csv"
#' nneo_file(product_code = "DP1.00098.001", site_code = "HEAL",
#'   year_month = "2016-05", filename = fname)
#'
#' ## curl options
#' nneo_data(product_code = "DP1.00098.001", site_code = "HEAL",
#'   year_month = "2016-05", verbose = TRUE)
#' }
nneo_data <- function(product_code, site_code, year_month, package = NULL, ...){
  res <- neon_parse(
    nGET(
      file.path(neon_base(), "data", product_code, site_code, year_month),
      query = jc(list(package = package)),
      ...
    )
  )
  res$data$files <- tibble::as_data_frame(res$data$files)
  res
}

#' @export
#' @rdname nneo_data
nneo_file <- function(product_code, site_code, year_month, filename, ...) {
  res <- nGET(
    file.path(neon_base(), "data", product_code, site_code,
              year_month, filename),
    ...
  )
  tibble::as_data_frame(
    data.table::fread(input = res, stringsAsFactors = FALSE, data.table = FALSE)
  )
}
