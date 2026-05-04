#' Resolve study- and site-level RCMEM defaults
#'
#' Reads optional RDS files containing named lists of RCMEM arguments.
#' Site defaults override study defaults.
#'
#' @param settings Run-specific PEcAn settings object.
#' @return Named list of resolved defaults.
#' @keywords internal
#' @export
resolve_defaults.RCMEM <- function(settings) {
  
  read_defaults <- function(path) {
    
    if (is.null(path) || length(path) == 0L) {
      return(list())
    }
    
    path <- unlist(path, use.names = FALSE)
    
    if (length(path) != 1L ||
        is.na(path) ||
        !nzchar(path)) {
      stop("Invalid RCMEM defaults path.")
    }
    
    if (!file.exists(path)) {
      stop("RCMEM defaults file not found: ", path)
    }
    
    x <- readRDS(path)
    
    if (!is.list(x) ||
        is.data.frame(x) ||
        is.null(names(x)) ||
        anyNA(names(x)) ||
        any(!nzchar(names(x))) ||
        anyDuplicated(names(x))) {
      stop(
        "RCMEM defaults must be a uniquely named list: ",
        path
      )
    }
    
    x
  }
  
  study <- read_defaults(
    settings$run$rcmem_defaults_path
  )
  
  site <- read_defaults(
    settings$run$site$site_defaults_path
  )
  
  # Site defaults override study defaults
  utils::modifyList(study, site)
}
