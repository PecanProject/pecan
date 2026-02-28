##' Update method for adaptive MCMC jump-tuning objects
##'
##' Adapts the jump standard deviation based on recent acceptance rate
##' to keep acceptance near the target rate.
##'
##' @param object object of class \code{jump}
##' @param chain numeric matrix of recent MCMC chain values used to compute acceptance rate
##' @param ... additional arguments (currently unused)
##' @return Updated \code{jump} object with adjusted history and acceptance rate.
##' @author Michael Dietze
##' @export
update.jump <- function(object, chain, ...) {
  jmp <- object
  ## check for valid typing
  if (is.null(jmp)) {
    stop("jump is NULL")
  }
  
  ## update counter
  cnt <- attr(jmp, "count") + 1
  attr(jmp, "count") <- cnt
  clen <- attr(jmp, "clen")
  
  ## update jump parm
  if (cnt%%clen == 0) {
    a <- max(arate(chain[(cnt - clen + 1):cnt, ]), 1 / clen)
    l <- length(attr(jmp, "history"))
    j <- attr(jmp, "history")[l]
    attr(jmp, "history")[l + 1] <- j * a / attr(jmp, "target")
    attr(jmp, "arate")[l + 1] <- a
  }
  jmp
}

## multivariate version
##' Update method for adaptive MCMC multivariate jump-tuning objects
##'
##' Adapts each dimension of the multivariate jump standard deviation based on
##' recent acceptance rate for each dimension to keep acceptance near the target rate.
##'
##' @param object object of class \code{mvjump}
##' @param chain numeric matrix of recent MCMC chain values used to compute acceptance rates
##' @param ... additional arguments (currently unused)
##' @return Updated \code{mvjump} object with adjusted history and acceptance rates.
##' @author Michael Dietze
##' @export
update.mvjump <- function(object, chain, ...) {
  jmp <- object
  ## check for valid typing
  if (is.null(jmp)) {
    stop("jump is NULL")
  }
  
  ## update counter
  cnt <- attr(jmp, "count") + 1
  attr(jmp, "count") <- cnt
  clen <- attr(jmp, "clen")
  
  ## update jump parm
  if (cnt %% clen == 0) {
    hnew <- rep(NA, ncol(chain))
    l <- nrow(attr(jmp, "history"))
    for (i in seq_along(chain)) {
      # print(c(i,cnt,clen))
      a <- arate(chain[(cnt - clen + 1):cnt, i])
      j <- attr(jmp, "history")[l, i]
      hnew[i] <- j * a / attr(jmp, "target")
    }
    # print(hnew)
    attr(jmp, "history") <- rbind(attr(jmp, "history"), hnew)
    attr(jmp, "arate")[l + 1] <- a
  }
  return(jmp)
} # update.mvjump
