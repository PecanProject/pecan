#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##'
##' Plant leaf reflectance and transmittance are calculated from 400 nm to
##' 2500 nm (1 nm step) with the following parameters: 
##'
##' @name prospect4
##' @title PROSPECT-4 leaf radiative transfer model
##' @param N leaf structure parameter.  Number of elementary layers
##' @param Cab leaf chlorophyll a+b content in ug/cm2
##' @param Cw leaf equivalent water thickness (EWT) in g/cm2 or cm-1
##' @param Cm leaf dry matter content in g/cm2 (alias leaf mass per area [LMA])
##'
##' @import gsl
##' @export
##' @examples
##' LRT <- prospect4(2,65,0.004,0.002)
##'
##' @references Stokes G.G. (1862), On the intensity of the light reflected from or transmitted through a pile of plates, Proc. Roy. Soc. Lond., 11:545-556.
##' @references Allen W.A., Gausman H.W., Richardson A.J., Thomas J.R. (1969), Interaction of isotropic ligth with a compact plant leaf, J. Opt. Soc. Am., 59(10):1376-1379.
##' @references Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G., Hosgood B. (1996), Estimating leaf biochemistry using the PROSPECT leaf optical properties model, Remote Sens. Environ., 56:194-202.
##' @references Jacquemoud S., Baret F. (1990), PROSPECT: a model of leaf optical properties spectra, Remote Sens. Environ., 34:75-91.
##' @references Feret et al. (2008), PROSPECT-4 and 5: Advances in the Leaf Optical Properties Model Separating Photosynthetic Pigments, Remote Sensing of Environment
##' @references Feret et al. (2008). http://teledetection.ipgp.jussieu.fr/prosail/
##' @author Shawn Serbin
##'
prospect4 <- function(N,Cab,Cw,Cm){
  
  # Here are some examples observed during the LOPEX'93 experiment on
  # fresh (F) and dry (D) leaves :
  #
  # ---------------------------------------------
  #                N     Cab     Cw        Cm    
  # ---------------------------------------------
  # min          1.000    0.0  0.004000  0.001900
  # max          3.000  100.0  0.040000  0.016500
  # corn (F)     1.518   58.0  0.013100  0.003662
  # rice (F)     2.275   23.7  0.007500  0.005811
  # clover (F)   1.875   46.7  0.010000  0.003014
  # laurel (F)   2.660   74.1  0.019900  0.013520
  # ---------------------------------------------
  # min          1.500    0.0  0.000063  0.0019
  # max          3.600  100.0  0.000900  0.0165
  # bamboo (D)   2.698   70.8  0.000117  0.009327
  # lettuce (D)  2.107   35.2  0.000244  0.002250
  # walnut (D)   2.656   62.8  0.000263  0.006573
  # chestnut (D) 1.826   47.7  0.000307  0.004305
  # ---------------------------------------------
  
  ### Load the spec. abs. features
  data(dataSpec_p4)
  
  l <- dataSpec_p4[,1]
  n <- dataSpec_p4[,2]
  
  ### Global absorption feature
  k <- (Cab*dataSpec_p4[,3]+Cw*dataSpec_p4[,5]+Cm*dataSpec_p4[,6])/N
  eps <- k[which(k==0)]
  
  exp.t <- function(x) exp(-x) / x
  expint_E1 <- function(k) sapply(k, function(x) integrate(exp.t, x, Inf)$value)
  
  trans <- (1-k)*exp(-k)+k^2*expint_E1(k) ### global trans
  tav <- function(alpha, n){
    alpha <- alpha * pi/180
    
    np <- n^2 + 1
    nm <- n^2 - 1
    a <- ((n + 1)^2) / 2
    k <- (-(n^2 - 1)^2) / 4
    sa <- sin(alpha)
    
    if(alpha == 0) {
      out <- 4 * n / (n+1)^2
      return(out)
    } else if(alpha == pi/2) {
      b1 <- rep(0, length(n))
    } else {
      b1 <- sqrt((sa^2 - np/2)^2 + k)
    }
    
    b2 <- sa^2 - np/2
    b <- b1 - b2
    
    t.s <- (k^2/(6*b^3) + k/b - b/2) - (k^2/(6*a^3) + k/a - a/2)
    t.p1 <- -2*n^2 * (b - a) / np^2
    t.p2 <- -2*n^2 * np * log(b/a) / nm^2
    t.p3 <- n^2 * (1/b - 1/a) / 2
    t.p4 <- 16*n^4 * (n^4 + 1) * log((2*np*b - nm^2)/(2*np*a - nm^2)) / (np^3 * nm^2)
    t.p5 <- 16*n^6 * (1/(2*np*b - nm^2) - 1/(2*np*a - nm^2)) / np^3
    t.p <- t.p1 + t.p2 + t.p3 + t.p4 + t.p5
    
    out <- (t.s + t.p) / (2*sa^2)
    
    return(out)
  }
  
  
  ### reflectivity and transmissivity at the interface. Leaf surface at 90 and 40 deg ang
  #-------------------------------------------------
  alpha <- 40
  t12 <- tav(alpha,n)       #trans
  t21 <- (tav(90,n))/n^2    #trans
  r12 <- 1-t12              #refl
  r21 <- 1-t21              #refl
  x <- (tav(alpha,n))/tav(90,n)
  y <- x*(tav(90,n)-1)+1-tav(alpha,n)

  ### reflectance and transmittance of the elementary layer N = 1
  #------------------------------------------------------------
  ra <- r12+(t12*t21*r21*trans^2)/(1-r21^2*trans^2)
  ta <- (t12*t21*trans)/(1-r21^2*trans^2)
  r90 <- (ra-y)/x
  t90 <- ta/x
  
  #***********************************************************************
  # reflectance and transmittance of N layers
  #***********************************************************************
  delta <- sqrt((t90^2-r90^2-1)^2-4*r90^2)
  beta <- (1+r90^2-t90^2-delta)/(2*r90)
  va <- (1+r90^2-t90^2+delta)/(2*r90)
  
  if (any(va*(beta-r90)<=1e-14)) {
    vb <- sqrt(beta*(va-r90)/(1e-14))
  } else {
    vb <- sqrt(beta*(va-r90)/(va*(beta-r90)))
  }

  ### Calc over N layers
  vbNN <- vb^(N-1)
  vbNNinv <- 1/vbNN
  vainv <- 1/va
  s1 <- ta*t90*(vbNN-vbNNinv)
  s2 <- ta*(va-vainv)
  s3 <- va*vbNN-vainv*vbNNinv-r90*(vbNN-vbNNinv)
  
  ### Calculate output reflectance and transmittance of the modeled leaf
  RN <- ra+s1/s3
  TN <- s2/s3
  LRT <- data.frame(Wavelength=l,
                    Reflectance=RN,
                    Transmittance=TN) # Output: wavelength, reflectance, transmittance
  
  return(LRT)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################