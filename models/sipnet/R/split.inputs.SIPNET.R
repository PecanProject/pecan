## split clim file into smaller time units to use in KF
##' @title split.inputs.SIPNET
##' @name  split.inputs.SIPNET
##' @author Mike Dietze and Ann Raiho
##' 
##' @param multi.settings
##' @param start.time
##' @param stop.time
##' @description Splits climate met for SIPNET
##' 
##' @return file split up climate file
##' @export
##' 
split.inputs.SIPNET <- function(settings,start.time,stop.time){
  
  start.time<-strftime(start.time,"%Y")
  stop.time<-strftime(stop.time,"%Y")
  
  full.met <- c(settings$run$inputs$met$path)
  new.met  <- file.path(settings$rundir,basename(full.met))
  file.copy(full.met,new.met)
  met <- new.met
  
  path <- dirname(met)
  prefix <- sub(".clim","",basename(met),fixed=TRUE)
  dat <- read.table(met,header=FALSE)
  file <- NA
  names(file) <- paste(start.time,"-",stop.time)
  sel <- which(dat[,2] == as.vector(start.time:stop.time))
  file<- paste(path,"/",prefix,".",paste(start.time,"-",stop.time),".clim",sep="") 
  write.table(dat[sel,],file,row.names=FALSE,col.names=FALSE)
  
  inputs$met$path<-file
  
  return(inputs)
  
}
