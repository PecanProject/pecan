spinup.LINKAGES <- function(start.year,end.year,temp.mat,precip.mat,paleon=NULL){
  if(is.null(paleon)) paleon = TRUE
  if(paleon==TRUE){
    spin.num <- 20
    start.year <- start.year - 1000
    
    temp.mat = rbind(temp.mat[rep(1:spin.num,length = 1000),],temp.mat)
    precip.mat = rbind(precip.mat[rep(1:spin.num,length=1000),],precip.mat) 
    nyear <- nrow(temp.mat)
    
  } else {
    spin.num <- round(length(start.year:end.year)/2)
    start.year <- start.year - spin.num
    year <- seq(start.year,end.year,1)
    nyear <- length(year)
    
    temp.mat = rbind(temp.mat[1:spin.num,],temp.mat)
    precip.mat = rbind(precip.mat[1:spin.num,],precip.mat)
  }

  return(list(start.year=start.year,nyear=nyear,temp.mat=temp.mat,precip.mat=precip.mat))
  
}