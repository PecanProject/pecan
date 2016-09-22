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
  
  
  start.day<-strftime(start.time, format = "%j")
  start.year<-strftime(start.time,"%Y")
  
  end.day<-strftime(stop.time, format = "%j")
  end.year<-strftime(stop.time,"%Y")
  
  full.met <- c(settings$run$inputs$met$path)
  new.met  <- file.path(settings$rundir,basename(full.met))
  file.copy(full.met,new.met)
  met <- new.met
  
  path <- dirname(met)
  prefix <- sub(".clim","",basename(met),fixed=TRUE)
  dat <- read.table(met,header=FALSE)
  file <- NA
  names(file) <- paste(start.time,"-",stop.time)
  
  sel1 <- which(dat[,2] == as.numeric(start.year) & dat[,3] == as.numeric(start.day))[1]
  sel2 <- which(dat[,2] == as.numeric(end.year) & dat[,3] == as.numeric(end.day))[length(which(dat[,2] == as.numeric(end.year) & dat[,3] == as.numeric(end.day)))]
  
  file<- paste(path,"/",prefix,".",paste0(as.Date(start.time),"-",as.Date(stop.time)),".clim",sep="") 
  
  write.table(dat[sel1:sel2,],file,row.names=FALSE,col.names=FALSE)
  
  settings$run$inputs$met$path<-file
  inputs<-settings$run$inputs
  
  return(inputs)
  
}
