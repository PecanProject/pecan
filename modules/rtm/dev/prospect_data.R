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
##' Transmission of isotropic radiation across an interface between two dielectrics. The
##' computation of the transmittivity at the leaf surface for a given incidence solid angle 
##' within PROSPECT 
##' 
##' @name tav
##' @title Transmission of radiation through elementary layers within PROSPECT
##' @param teta angle
##' @param ref refractive index value by wavelength
##' 
##' @references Stern F. (1964), Transmission of isotropic radiation across an interface between two dielectrics, Appl. Opt., 3(1):111-113.
##' @references Allen W.A. (1973), Transmission of isotropic light across a dielectric surface in two and three dimensions, J. Opt. Soc. Am., 63(6):664-666.
##' @references Feret et al. (2008), PROSPECT-4 and 5: Advances in the Leaf Optical Properties Model Separating Photosynthetic Pigments, Remote Sensing of Environment
##' @references Feret et al. (2008). http://teledetection.ipgp.jussieu.fr/prosail/
##' @author Shawn Serbin
##' 
tav.f <- function(teta,ref){ 
  ### Based on Feret et al., (2008).  Source code
  ### downloaded from http://teledetection.ipgp.jussieu.fr/prosail/
  
  s <- length(ref)
  teta <- teta*pi/180
  r2 <- ref^2
  rp <- r2+1
  rm <- r2-1
  a <- ((ref+1)^2)/2
  k <- (-(r2-1)^2)/4
  ds <- sin(teta)
  
  k2 <- k^(2)
  rm2 <- rm^2
  
  if (teta==0){
    f <- 4*ref/(ref+1)^2
  } else if (teta==pi/2){
    b1 <- rep(0,1,s)
  } else {
    b1 <- sqrt((ds^2-rp/2)^2+k)
  }
  
  b2 <- ds^2-rp/2
  b <- b1-b2
  ts <- (k2/(6*b^3)+k/b-b/2)-(k2/(6*a^3)+k/a-a/2)
  tp1 <- -2*r2*(b-a)/(rp^2)
  tp2 <- -2*r2*rp*log(b/a)/rm2
  tp3 <- r2*(b^(-1)-a^(-1))/2
  tp4 <- 16*r2^(2)*(r2^2+1)*log((2*rp*b-rm2)/(2*rp*a-rm2))/(rp^(3)*rm2)
  tp5 <- 16*r2^(3)*((2*rp*b-rm2)^(-1)-(2*rp*a-rm2)^(-1))/rp^3
  tp <- tp1+tp2+tp3+tp4+tp5
  f <- (ts+tp)/(2*ds^2)
  return(f)
  
}

## Generate data file for given prospect model
prospect.datamatrix <- function(model){
	if (model == "prospect4"){
		data(dataSpec_p4)
		dat <- dataSpec_p4[,-4]    # Drop empty Car absorption features column
	}
	else if (model == "prospect5"){
		data(dataSpec_p5)
		dat <- dataSpec_p5
	}
	else if (model == "prospect5b"){
		data(dataSpec_p5B)
		dat <- dataSpec_p5B
	}
	nd <- ncol(dat)
	nr <- dat$refractive_index
	t90 <- tav.f(90, nr)
	tav <- tav.f(40, nr)
	tao1 <- tav
	tao2 <- t90 / nr^2
	rho1 <- 1 - tao1
	rho2 <- 1 - tao2
	x <- tav / t90
	y <- x * (t90 - 1) + 1 - tav
	pdat <-  as.matrix(cbind(dat[,3:nd], tao1, tao2, rho1, rho2, x, y))	
	return(pdat)
}

#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.            	
####################################################################################################
