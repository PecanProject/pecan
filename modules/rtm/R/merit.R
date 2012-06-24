#--------------------------------------------------------------------------------------------------#
##'
##' @name merit.p4
##' @title Merit function for inverting PROSPECT-4 on observed leaf reflectance and transmittance data
##'
##'
##' @author Shawn Serbin
merit.p4 <- function(x){
  N <- x[1]
  Cab <- x[2]
  Cw <- x[3]
  Cm <- x[4]
  LRT <- prospect4(N,Cab,Cw,Cm)
  
  ### Define merit function
  #rmse <- sqrt(sum(((LRT[,2]-refl)^2) + ((LRT[,3]-tran)^2)))
  rmse <- sqrt((sum(((LRT[,2]-refl)^2) + ((LRT[,3]-tran)^2)))/2101)
  
}
#==================================================================================================#


#--------------------------------------------------------------------------------------------------#
##'
##' @name merit.p5
##' @title Merit function for inverting PROSPECT-5 on observed leaf reflectance and transmittance data
##'
##'
##' @author Shawn Serbin
merit.p5 <- function(x){
  N <- x[1]
  Cab <- x[2]
  Car <- x[3]
  Cw <- x[4]
  Cm <- x[5]
  LRT <- prospect5(N,Cab,Car,Cw,Cm)
  
  ### Define merit function
  rmse <- sqrt((sum(((LRT[,2]-refl)^2) + ((LRT[,3]-tran)^2)))/2101)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################