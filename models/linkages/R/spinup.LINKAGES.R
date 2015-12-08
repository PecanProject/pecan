spinup.LINKAGES <- function(start.year,end.year,temp.mat,precip.mat){
  spin.num <- round(length(start.year:end.year)/2)
  start.year <- start.year - spin.num
  year <- seq(start.year,end.year,1)
  nyear <- length(year)
  
  temp.mat = rbind(temp.mat[1:spin.num,],temp.mat)
  precip.mat = rbind(precip.mat[1:spin.num,],precip.mat)
  
  return(list(start.year=start.year,nyear=nyear,temp.mat=temp.mat,precip.mat=precip.mat))
  
}