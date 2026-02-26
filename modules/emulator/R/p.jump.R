##' Extract current jump parameter value for a \code{jump} object
##'
##' @title p.jump
##' @param x object of class \code{jump}
##' @param ... additional arguments (currently unused)
##' @return The most recent jump parameter value.
##' @author Michael Dietze
##' @exportS3Method PEcAn.emulator p
p.jump <- function(x, ...) {
  jmp <- x
  n <- length(attr(jmp, "history"))
  return(attr(jmp, "history")[n])
}

##' Extract current jump parameter values for a \code{mvjump} object
##'
##' @title p.mvjump
##' @param x object of class \code{mvjump}
##' @param ... additional arguments (currently unused)
##' @return The most recent row of the multivariate jump parameter history.
##' @author Michael Dietze
##' @exportS3Method PEcAn.emulator p
p.mvjump <- function(x, ...) {
  jmp <- x
  n <- nrow(attr(jmp, "history"))
  return(attr(jmp, "history")[n, ])
} # p.mvjump
