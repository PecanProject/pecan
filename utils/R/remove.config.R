remove.config <- function(dir,settings,model){

  fcn.name <- paste("remove.config.",model,sep="")
  if(exists(fcn.name)){
    do.call(fcn.name,args=list(dir,settings))
  } else {
    warning(paste(fcn.name,"does not exist"))
    warning("This function is not required, but it's implementation is recommended")
  }

  
}
