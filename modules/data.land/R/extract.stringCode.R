extract.stringCode <- function(x,extractor=from.TreeCode){
  return(extractor(x))
}

from.TreeCode <- function(x){
  SITE = substr(x,1,1)
  PLOT = substr(x,2,2)
  SUBPLOT = substr(x,3,3)
  TAG = substr(x,4,1000000L)
  return(data.frame(SITE,PLOT,SUBPLOT,TAG))
}

to.TreeCode <- function(SITE,PLOT,SUBPLOT,TAG=NULL){
  SITE = as.character(SITE)
  PLOT = as.character(PLOT)
  SUBPLOT = as.character(SUBPLOT)
  TAG = as.character(TAG)
  x = paste(SITE,PLOT,SUBPLOT,sep="")
  if(!is.null(x)){
    x = paste(x,TAG,sep="")
  }
  return(x)
}

from.Tag <- function(x){
  miss = rep(NA,length(x))
  return(data.frame(SITE=miss,PLOT=miss,SUBPLOT=miss,TAG=x))
}

to.Tag <- function(SITE,PLOT,SUBPLOT,TAG=NULL){
  SITE = as.character(SITE)
  PLOT = as.character(PLOT)
  SUBPLOT = as.character(SUBPLOT)
  TAG = as.character(TAG)
  return(TAG)
}