#' Check individual ED input files
#'
#' Check internal file formatting, and optionally check for compatibility 
#' against related files.
#'
#' @param css css data object (see [read_css])
#' @param pss pss data object (see [read_pss])
#' @param site site data object (see [read_site])
#' @return `NULL` (invisibly)
#' @export
check_css <- function(css, pss = NULL) {
  conditions <- expression(
    # Only these columns are present, exactly in this order
    identical(
      colnames(css),
      c("time", "patch", "cohort", "dbh", "hite", "pft", "n", "bdead", "balive", "lai")
    ),
    is.data.frame(css),
    nrow(css) >= 1
  )

  if (!is.null(pss)) {
    pss_conditions <- expression(
      # All cohort patches are defined in patch file
      all(unique(css$patch) %in% unique(pss$patch))
    )
  } else {
    pss_conditions <- NULL
  }

  test_conditions(c(conditions, pss_conditions))
}

#' @rdname check_css
#' @export
check_pss <- function(pss, site = NULL) {
  conditions <- expression(
    is.data.frame(pss),
    nrow(pss) >= 1
  )

  if (!is.null(site)) {
    site_conditions <- expression(
      all(unique(pss$site) %in% unique(site$site))
    )
  } else {
    site_conditions <- NULL
  }
  
  test_conditions(c(conditions, site_conditions))
}

#' @rdname check_css
#' @export
check_site <- function(site) {
  conditions <- expression(
    nrow(site) >= 1,
    !is.null(attributes(site)),
    is.numeric(attr(site, "nsite")),
    attr(site, "file_format") %in% c(1, 2, 3)
  )
  
  test_conditions(conditions)
}

#' Test list of conditions
#'
#' Evaluate a list of logical conditions, and throw an informative error if any 
#' are `FALSE`.
#' @param conditions Vector of [expressions][base::expression].
#' @return `NULL` (invisibly)
test_conditions <- function(conditions) {
  results <- Reduce(c, Map(eval, conditions))
  if (any(!results)) {
    errors <- lapply(conditions[!results], deparse, width.cutoff = 100L)
    error_string <- paste(errors, collapse = "; ")
    PEcAn.logger::logger.severe(
      "The following conditions were not met: ",
      error_string
    )
  }
  invisible(NULL)
}

