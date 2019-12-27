#' @title Calculate Gelman diagnostic on moving window
#'
#' @author Alexey Shiklomanov
#' @param x MCMC samples, of class \code{mcmc} or \code{mcmc.list}
#' @param width_fraction Fractional width of moving window. Default=0.1.
#' @param width Width of moving window. Default is niter(x)*width_fraction
#' @param njump Number of windows to calculate over
#' @param include.mpsrf Whether to calculate multivariate PSRF and include in output (default = FALSE).
#' @return Gelman Diagnostic 3D array. First dim -- mean (1) and 95% confidence (2). Second dim -- iteration
#' @export
gelman_diag_mw <- function(x,
                           width_fraction = 0.1,
                           width = ceiling(coda::niter(x)*width_fraction),
                           njump = 50,
                           include.mpsrf = TRUE,
                           ...) {

  stopifnot(inherits(x, c("mcmc", "mcmc.list")))
  stopifnot(width %% 1 == 0)
  stopifnot(njump %% 1 == 0)
  startx <- stats::start(x)
  endx <- stats::end(x)
  a <- floor(seq(startx, endx - width + 1, length.out = njump))
  b <- ceiling(seq(startx + width - 1, endx, length.out = njump))
  if (length(a) < 1) {
    stop("Start index vector has length 0")
  }
  if (length(b) < 1) {
    stop("End index vector has length 0")
  }
  if (length(a) != length(b)) {
    stop("Start and end index vector length mismatch.\n",
         "Start length = ", length(a), "\n",
         "End length = ", length(b))
  }
  n_row <- length(a)
  n_col <- coda::nvar(x) + 2
  vnames <- coda::varnames(x)
  if (is.null(vnames)) {
    vnames <- paste0("V", seq_len(coda::nvar(x)))
  }
  col_names <- c("Start", "End", vnames)
  if (include.mpsrf) {
    n_col <- n_col + 1
    col_names <- c(col_names, "mpsrf")
  }
  gdmat <- array(numeric(), c(n_row, n_col, 2))
  dimnames(gdmat)[[2]] <- col_names
  gdmat[,1,] <- a
  gdmat[,2,] <- b
  for (i in seq_len(n_row)) {
    xsub <- stats::window(x, start=a[i], end=b[i])
    gd_raw <- coda::gelman.diag(xsub, 
                                autoburnin=FALSE,
                                multivariate = include.mpsrf)
    gd <- gd_raw$psrf
    if (include.mpsrf) {
      gd <- rbind(gd, "mpsrf" = rep(gd_raw$mpsrf, 2))
    }
    gdmat[i, -(1:2), ] <- gd
  }
  return (gdmat)
} # gelman_diag_mw

#' @title Calculate Gelman Diagnostic using coda::gelman.plot
#' 
#' @author Alexey Shiklomanov
#' @inheritParams x
#' @description Calculates Gelman diagnostic cumulatively. This is a much 
#' more conservative approach than the moving-window method.
#' @export
gelman_diag_gelmanPlot <- function(x, ...) {
  grDevices::pdf(file = NULL)
  GBR_raw <- coda::gelman.plot(x)
  grDevices::dev.off()
  GBR <- array(numeric(), dim(GBR_raw$shrink) + c(0, 2, 0))
  dimnames(GBR)[[2]] <- c("Start", "End", dimnames(GBR_raw$shrink)[[2]])
  GBR[,-(1:2),] <- GBR_raw$shrink
  GBR[, 2, ] <- GBR_raw$last.iter
  GBR[, 1, 1] <- GBR[, 1, 2] <- c(1, GBR[-nrow(GBR), 2, 1] + 1)
  return(GBR)
}

