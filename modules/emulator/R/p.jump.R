#--------------------------------------------------------------------------------------------------#
##' 
##' @title p.jump 
##' @export
##'

`p.jump` <-
function(jmp){
  n <- length(attr(jmp,"history"))
  return(attr(jmp,"history")[n])
}

`p.mvjump` <-
function(jmp){
  n <- nrow(attr(jmp,"history"))
  return(attr(jmp,"history")[n,])
}

