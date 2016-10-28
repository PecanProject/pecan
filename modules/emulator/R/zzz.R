##' @name jump
##' @title setClass jump
##' @export
##' 
##' @author Michael Dietze

# .First.lib <- function(which.lib.loc, package){

## define a class for automatically tuning jump distributions
methods::setClass("jump", methods::representation(history = "numeric", count = "numeric", target = "numeric", 
                                clen = "numeric", arate = "numeric"), 
         prototype = list(history = vector("numeric", 0),  count = 0, target = 0.4, 
                          clen = 100, arate = vector("numeric", 0)))
## chain = mcmc chain history = jump parm history count = counter to update jump parm
## target = target acceptance rate clen = update period (recompute when count > clen)
methods::setIs("jump", "list")


##' @name mvjump
##' @title setClass mvjump 
##' @export
##' 
methods::setClass("mvjump", methods::representation(history = "matrix", count = "numeric", target = "numeric", 
                                  clen = "numeric", arate = "numeric", mydim = "numeric"), 
         prototype = list(history = matrix(NA, 0, 0), count = 0, target = 0.4, 
                          clen = 100, arate = vector("numeric", 0), mydim = 1))
## history = jump parm history count = counter to update jump parm target = target
## acceptance rate clen = update period (recompute when count > clen)
methods::setIs("mvjump", "list")

# }
