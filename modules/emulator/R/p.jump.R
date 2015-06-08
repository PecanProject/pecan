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


# p.jump <-
# function(jmp){
#   n <- nrow(jmp$history)
#   return(jmp$history[n,])
# }
