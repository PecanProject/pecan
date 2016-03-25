##' @name p.jump
##' @title p.jump
##' @export
##' 
##' @param jmp jump parameter
##' 
##' @author Michael Dietze
`p.jump` <-
function(jmp){
  n <- length(attr(jmp,"history"))
  return(attr(jmp,"history")[n])
}

##' @name p.mvjump
##' @title p.mvjump
##' @export
##' 
##' @param jmp jump parameter
##' 
`p.mvjump` <-
function(jmp){
  n <- nrow(attr(jmp,"history"))
  return(attr(jmp,"history")[n,])
}

