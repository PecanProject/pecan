##' Extract current jump parameter value for a \code{jump} object
##'
##' @param x object of class \code{jump}
##' @param ... additional arguments (currently unused)
##' @return The most recent jump parameter value.
##' @author Michael Dietze
##' @export
p.jump <- function(x, ...) {
  jmp <- x
  n <- length(attr(jmp, "history"))
  return(attr(jmp, "history")[n])
}

##' Extract current jump parameter values for a \code{mvjump} object
##'
##' @param x object of class \code{mvjump}
##' @param ... additional arguments (currently unused)
##' @return The most recent row of the multivariate jump parameter history.
##' @author Michael Dietze
##' @export
p.mvjump <- function(x, ...) {
  jmp <- x
  n <- nrow(attr(jmp, "history"))
  return(attr(jmp, "history")[n, ])
} # p.mvjump
