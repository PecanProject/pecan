#' Effective sample size
#'
#' Calculate effective sample size of vector based on its autocorrelation.
#' @param x A vector or time series
#' @param ... additional arguments passed to methods
#' @export
neff <- function(x, ...) {
  UseMethod("neff")
}

#' @export
neff.default <- function(x, lag.max = NULL, min_rho = 0.1) {
    x_use <- x[!is.na(x)]
    nx <- length(x_use)
    if (is.null(lag.max)) {
        # Same as in the ACF function
        lag.max <- floor(10 * log10(nx))
    }
    rho_all <- .Call(stats:::C_acf, x_use, lag.max, TRUE)
    rho <- rho_all[-1]
    too_small <- rho < min_rho
    if (any(too_small)) {
        rho <- rho[seq_len(which(too_small)[1])]
    }
    nrho <- length(rho)
    tau <- 1 + 2 * sum((1 - seq_len(nrho) / nx) * rho)
    n_eff <- nx / tau
    return(n_eff)
}

#' @export
neff.matrix <- function(x, ...) {
  col_neff <- apply(x, 2, neff.default, ...)
  return(sum(col_neff))
}

# Calculate max ACF lag from correlation power analysis
corr_max_lag <- function(nx, r = 0.1, sig.level = 0.05, power = 0.95, ...) {
    testForPackage('pwr')
    power_analysis <- pwr::pwr.r.test(n = NULL, r = r, sig.level = sig.level, power = power, ...)
    nlag <- ceiling(nx - power_analysis$n)
    return(nlag)
}
