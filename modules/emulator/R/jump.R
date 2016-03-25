##' @name jump
##' @title jump
##' @export
##'
##' @param ic
##' @param rate
##' 
##' @author Michael Dietze
`jump` <-
function(ic=0,rate=0.4,...){
  return(new("jump",history=ic,arate=0,target=rate))
}

##' multivariate version
##' @title mvjump 
##' @export

`mvjump` <-
function(ic=0,rate=0.4,nc=2,...){
  icm <- (matrix(ic,nrow=1,ncol=nc))
  return(new("mvjump",history=icm,arate=0,target=rate))
}

