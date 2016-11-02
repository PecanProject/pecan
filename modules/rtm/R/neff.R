#' @title Effective sample size
#'
#' @description Calculate effective sample size of vector based on its 
#' autocorrelation.
#' @param x A vector or time series
#' @export
neff <- function(x) {
  UseMethod("neff")
}

neff.default <- function(x) {
    xna <- is.na(x)
    if (any(xna)) {
        warning("NA in neff input. Omitting.")
        x <- x[!xna]
    }
    arout <- ar.yw(x)
    spec <- arout$var.pred/(1 - sum(arout$ar))^2
    out <- length(x) * var(x) / spec
    stopifnot(length(out) == 1)
    return(out)
}

neff.matrix <- function(x) {
  col_neff <- apply(x, 2, neff.default)
  return(mean(col_neff))
}

