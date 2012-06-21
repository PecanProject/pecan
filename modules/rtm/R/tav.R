#--------------------------------------------------------------------------------------------------#
##'
##' Transmission of isotropic radiation across an interface between two dielectrics
##' The transmission of radiation through elementary layers within PROSPECT
##' @name tav
##' @title Transmission of radiation through elementary layers within PROSPECT
##' @param teta transmission
##' @param ref reflectance
##' 
##' @references Stern F. (1964), Transmission of isotropic radiation across an interface between two dielectrics, Appl. Opt., 3(1):111-113.
##' @references Allen W.A. (1973), Transmission of isotropic light across a dielectric surface in two and three dimensions, J. Opt. Soc. Am., 63(6):664-666.
##' @references Feret et al. (2008), PROSPECT-4 and 5: Advances in the Leaf Optical Properties Model Separating Photosynthetic Pigments, Remote Sensing of Environment
##' @references Feret et al. (2008). http://teledetection.ipgp.jussieu.fr/prosail/
##' @author Shawn Serbin
##' 
tav <- function(teta,ref){ 
  ### Code ported from Matlab.  Based on Feret et al., (2008).  Source
  ### code downloaded from http://teledetection.ipgp.jussieu.fr/prosail/
  
  s <- dim(ref)  # not sure if this should be dim or length yet
  teta <- teta*pi/180
  r2 <- ref^2
  rp <- r2+1
  rm <- r2-1
  a <- ((ref+1)^2)/2
  
  #not complete
  
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################