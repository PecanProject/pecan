##' Plot diagnostic history for a \code{jump} tuning object
##'
##' Displays the jump parameter trace and acceptance rate over MCMC iterations.
##'
##' @param x object of class \code{jump}
##' @param ... additional arguments (currently unused)
##' @author Michael Dietze
##' @export
plot.jump <- function(x, ...) {
  jmp <- x
  graphics::par(mfrow = c(1, 2))
  plot(attr(jmp, "history"), 
       ylab = "Jump Parameter",
       main = "Jump Parameter")
  graphics::abline(h = mean(attr(jmp, "history"), na.rm = TRUE))
  plot(attr(jmp, "arate"), 
       main = "Acceptance Rate", 
       ylim = c(0, 1), 
       ylab = "Acceptance Rate")
  graphics::abline(h = mean(attr(jmp, "arate"), na.rm = TRUE))
  graphics::abline(h = attr(jmp, "target"), col = 2)
} # plot.jump

##' Plot diagnostic history for a \code{mvjump} tuning object
##'
##' Displays the multivariate jump parameter trace and acceptance rate over MCMC iterations.
##'
##' @param x object of class \code{mvjump}
##' @param ... additional arguments (currently unused)
##' @author Michael Dietze
##' @export
plot.mvjump <- function(x, ...) {
  jmp <- x
  graphics::par(mfrow = c(1, 2))
  plot(
    attr(jmp, "history")[, 1],
    ylab = "Jump Parameter",
    main = "Jump Parameter")
  graphics::abline(h = mean(attr(jmp, "history")[, 1], na.rm = TRUE))
  graphics::text(
    0.9 * length(attr(jmp, "history")[, 1]),
    min(attr(jmp, "history")[, 1]) + 0.8 *
      (max(attr(jmp, "history")[, 1]) - min(attr(jmp, "history")[, 1])),
    paste("mean=", mean(attr(jmp, "history")[, 1])))
  plot(
    attr(jmp, "arate"),
    ylab = "Acceptance Rate",
    main = "Acceptance Rate",
    ylim = c(0, 1))
  graphics::abline(h = attr(jmp, "target"))
  graphics::abline(h = mean(attr(jmp, "arate"), na.rm = TRUE), col = 2)
} # plot.mvjump
