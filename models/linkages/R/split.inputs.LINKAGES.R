##' @title split.inputs.LINKAGES
##' @name  split.inputs.LINKAGES
##' @author Ann Raiho
##' 
##' @param multi.settings
##' @param start.time
##' @param stop.time
##' @description Splits climate met for LINKAGES
##' 
##' @return files split up climate files
##' @export
##' 
split.inputs.LINKAGES <- function(settings,start.time,stop.time){
  
  new.met <- paste0(settings$rundir,"/climate.Rdata") #doesn't do anything but write stuff to README
  settings$run$inputs$met$path <- new.met #HACK
  
  inputs<-settings$run$inputs

  return(inputs)
  
}