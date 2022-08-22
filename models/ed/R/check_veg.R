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
  if(!inherits(css, "data.frame") | nrow(css) == 0) {
    stop("css file should be a data frame")
  }
  
  if(colnames(css) != c("time", "patch", "cohort", "dbh", "hite", "pft",
                        "n", "bdead", "balive", "lai")) {
    stop("css file is formatted incorrectly")
  }
  
  if (!is.null(pss)) {
    if(!all(unique(css$patch) %in% unique(pss$patch))) {
      stop("css file and pss file are not compatible")
    }
  }
  
}

#' @rdname check_css
#' @export
check_pss <- function(pss, site = NULL) {
  testthat::test_that(
    "pss file is formatted correctly",
    {
      testthat::expect_is(pss, "data.frame")
      testthat::expect_gte(nrow(pss), 1)
    }
  )
  if (!is.null(site)) {
    testthat::test_that(
      "pss and site files are compatible",
      {
        testthat::expect_true(all(unique(pss$site) %in% unique(site$sitenum)))
      }
    )
  }
}

#' @rdname check_css
#' @export
check_site <- function(site) {
  testthat::test_that(
    "site file is formatted correctly",
    {
      testthat::expect_gte(nrow(site), 1)
      testthat::expect_true(!is.null(attributes(site)))
      testthat::expect_is(attr(site, "nsite"), "numeric")
      testthat::expect_true(attr(site, "file_format") %in% c(1, 2, 3))
    }
  )
}
