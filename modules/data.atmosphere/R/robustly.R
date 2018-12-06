#' Adverb to try calling a function `n` times before giving up
#'
#' @param .f Function to call.
#' @param n Number of attempts to try
#' @param timeout Timeout between attempts, in seconds
#' @param silent Silence error messages?
#' @return Modified version of input function
#' @examples
#' rlog <- robustly(log, timeout = 0.3)
#' try(rlog("fail"))
#' \dontrun{
#'  nc_openr <- robustly(ncdf4::nc_open, n = 10, timeout = 0.5)
#'  nc <- nc_openr(url)
#'  # ...or just call the function directly
#'  nc <- robustly(ncdf4::nc_open, n = 20)(url)
#'  # Useful in `purrr` maps
#'  many_vars <- purrr::map(varnames, robustly(ncdf4::ncvar_get), nc = nc)
#' }
#' @export
robustly <- function(.f, n = 10, timeout = 0.2, silent = TRUE) {
  .f <- purrr::as_mapper(.f)
  function(...) {
    attempt <- 1
    while (attempt <= n) {
      result <- try(.f(...), silent = silent)
      if (!inherits(result, "try-error")) return(result)
      attempt <- attempt + 1
      if (!silent) PEcAn.logger::logger.info("Trying attempt ", attempt, " of ", n)
    }
    PEcAn.logger::logger.severe("Failed after", n, "attempts.")
  }
}
