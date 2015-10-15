\name{Rpreles-package}
\alias{Rpreles-package}
\alias{Rpreles}
\docType{package}
\title{A model set for predicting ecosystem GPP and water balance.
  Model set for Climforisk Life+ project LIFE09 ENV FI 000571. 
}
\description{Implements PRELES carbon-water cycle model that is really simple. It
  calculates GPP following Mäkelä et al. 2008, GCB, with small
  modifications to VPD-response modifier, with the inclusion of soil water
  modifier. GPP prediction is used in an empirical water use-efficient
  based transpiration equation that is appended by Priestley-Taylor type
  of evaporation equation, which can be constrained by low soil
  water. Photosynthetic radiation absorbed by the canopy influences
  transpiration-evaporation fraction, each driven by photosynthetic
  radiation, instead of Rnet for the sake of simplicity of input data.
  Soil water balance is updated at the end of the day. Snow melts
  when it is warmer than zero degrees C, and rain is assumed water when
  air temperature is lower than that. There is a small surfacial canopy
  water storage that is simple associated with fAPAR, and if there is
  water soil water restrictions to evaporation do not apply.
  
  Model has been developed and calibrated in Peltoniemi et al. submitted
  for Hyytiälä and Sodankylä eddy covariance sites (Scots pine).
  
  Preliminary calibrations of the model show promising performance in
  other Finnish and Swedish eddy covariance sites, including Norway spruce
  too, implying it could be used elsewhere in boreal region as well. Use
  elsewhere with caution, as it anyway is an empirical model by its
  nature.
  
  There are options for simple extensions of model applicability:
  - LUE (and gamma) parameters are associated with photosynthetic
  capacisity, meaning LUE tends to be higher in deciduous. Note that it is
  correlated with gamma that (Peltoniemi et al 2012, Tree Phys.).
  - Seasonality controls deciduos species differently. Birch budburst is
  modelled according to Linkosalo et al. (2000?).
  - Spruce seems to have more sensitive fVPD-modifier than Scots pine,
  which means one might try to increase kappaGPP parameter.
}
\details{
  \tabular{ll}{
    Package: \tab Rpreles\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2014-2-13\cr
    License: \tab \cr
  }
  An R interface to PRELES (and possibly later to Yasso, and NPP An overview of how to use the package, including the most important functions.
}
\author{
  Mikko Peltoniemi
  
  Maintainer: Mikko Peltoniemi <mikko.peltoniemi@metla.fi>
}
\references{ 
  Peltoniemi et al., submitted 18.11.2013 
}

\keyword{ package }
\seealso{
  Optional links to other man pages, e.g.
  \code{\link[<pkg>:<pkg>-package]{<pkg>}} 
}
\examples{
##yasso examples by Sanna Härkönen
#init <- matrix(0,length(STAND0$id),5)
#z <- matrix(0,length(STAND0$id),5)
#nw.y07<-rep(0.0,length(STAND0$id)); fw.y07<-rep(0.0,length(STAND0$id)); w.y07<-rep(0.0,length(STAND0$id))
#t <- 10000 #time in years

#In steady state simulations: weather data mean values 1960-2008
#amp1_0 <- (STAND0$maxAVE-STAND0$minAVE)/2 #
#cl <- cbind(STAND0$meanAVE,STAND0$rainAVE, amp1_0) #(T_mean[C],P_annual[mm], T_amplitude[C])

# non-woody litter
#inf1 <-  cbind(non.woody0, 0)#Input per compartments (A, W, E, N, H)
#inf2 <-  cbind(fine.woody0, 0)#Input per compartments (A, W, E, N, H)
#inf3 <-  cbind(woody0, 0)#Input per compartments (A, W, E, N, H)

#input for Yasso
#inputnw=cbind(t,cl,init,inf1,s1,z,YassoParam) #input for Yasso (non-woody), YassoParam=1 denotes Scandinavian parameter set
#inputfw=cbind(t,cl,init,inf2,s2,z,YassoParam) #input for Yasso (fine-woody)
#inputw=cbind(t,cl,init,inf3,s3,z,YassoParam) #input for Yasso (woody)

#nw.y07 <- t(apply(inputnw,1,yasso07))
#fw.y07 <- t(apply(inputfw,1,yasso07))
#w.y07 <- t(apply(inputw,1,yasso07))
}
