spinup.LINKAGES <- function(start.year,end.year,temp.mat,precip.mat){
  start.year <- start.year - 500
  year <- seq(start.year,end.year,1)
  nyear <- length(year)
  
  temp.mat = rbind(temp.mat[1:(nrow(temp.mat)/2),],temp.mat)
  precip.mat = rbind(precip.mat[1:(nrow(temp.mat)/2),],precip.mat)
  
  return(list(start.year=start.year,nyear=nyear,temp.mat=temp.mat,precip.mat=precip.mat))
  
}