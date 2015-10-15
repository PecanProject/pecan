\name{PRELES}
\alias{PRELES}
\alias{preles}
\alias{Preles}
\title{A simple semi-empirical ecosystem carbon and water balance model.
}
\description{The model predicts gross primary production and evapotranpiration (and soil water balance) based on embedded empirical relationships and basic meteorological inputs.
}
\usage{ 
  PRELES(PAR, TAir, VPD, Precip, CO2, fAPAR, 
    GPPmeas = NA, ETmeas = NA, SWmeas = NA, 
      p = rep(NA, 30), DOY = NA, LOGFLAG = 0, control = 0,   
        returncols = c("GPP", "ET", "SW")) 
}
\arguments{
  \item{PAR}{A numeric vector of daily sums of photosynthetically active radiation, mmol/m2.  }
  \item{TAir}{A numeric vector of daily mean temperature, degrees C. }
  \item{VPD}{A numeric vector of daily mean vapour pressure deficits}
  \item{Precip}{A numeric vector of daily rainfall, mm}
  \item{CO2}{A numeric vector of air CO2}
  \item{fAPAR}{A numeric vector of fractions of absorbed PAR by the canopy, 0-1 unitless}
  
  
  ## OPTIONAL FOR BYPASSING PREDICTION. FOR DEVELOPMENT ONLY
  \item{GPPmeas}{NA}
  \item{ETmeas}{NA}
  \item{SWmeas}{NA}
  
  
  \item{p}{parameter vector of length 30. If parameter has value NA it is replaced with a default corresponding to Hyytiälä calibration. Vector p has following defaults:
    
    ## SITE AND SOIL RELATED
    soildepth = 413.0, ## 1 soildepth
    Effective field capacity = 0.450, ## 2 ThetaFC
    Permanent wilting point = 0.118, ## 3 ThetaPWP
    Drainage delay = 3 days, ## 4 tauDrainage
    
    ## GPP_MODEL_PARAMETERS
    LUE = 0.748018, ## 5 betaGPP
    tauGPP = 13.23383, ## 6 tauGPP
    S0GPP = -3.9657867, ## 7 S0GPP
    SmaxGPP = 18.76696, ## 8 SmaxGPP
    kappaGPP = -0.130473, ## 9 kappaGPP
    gammaGPP = 0.034459, ## 10 gammaGPP
    soilthresGPP = 0.450828, ## 11 soilthresGPP
    cmCO2 = 2000, ## 12 cmCO2
    ckappa = 0.4, ## 13 ckappaCO2
    ## EVAPOTRANSPIRATION_PARAMETERS
    transpiration efficiency = 0.324463, ## 14 betaET
    kappaET = 0.874151, ## 15 kappaET
    chiET = 0.075601, ## 16 chiET
    soilthresET = 0.541605, ## 17 soilthresET
    nuET = 0.273584, ## 18 nu ET
    ## SNOW_RAIN_PARAMETERS
    Metlcoeff = 1.2, ## 19 Meltcoef
    I_0 = 0.33, ## 20 I_0
    CWmax = 4.970496, ## 21 CWmax, i.e. max canopy water
    SnowThreshold = 0, ## 22 SnowThreshold, 
    T_0 = 0, ## 23 T_0, 
    ## Initialisation
    SWinit = 200, ## 24 SWinit, ## START INITIALISATION PARAMETERS 
    CWinit = 0, ## 25 CWinit, ## Canopy water
    SOGinit = 0, ## 26 SOGinit, ## Snow on Ground 
    Sinit = 20, ## 27 Sinit ##CWmax
    t0 = -999, ## t0 fPheno_start_date_Tsum_accumulation; conif -999, for birch 57
    tcrit =  -999, ## tcrit, fPheno_start_date_Tsum_Tthreshold, 1.5 birch
    tsumcrit = -999 ##tsumcrit, fPheno_budburst_Tsum, 134 birch
    
    
    
  }
  
  
  
  \item{DOY}{Day of year integer vector. Needed for the prediction of deciduous species phenology. If DOY is not provided and deciduous species phenology parameters are not set to -999, it is assumed that the first values of all input vectors are from 1st Jan, and year has 365 day.}

  \item{LOGFLAG}{levels 0 (default), 1, 2. Generates increasing logging to preles.log file in the run directory.}
  
  \item{control}{
    Parameter that selects the transpiration model. Equals etmodel in c-code.
    
    if (etmodel == 0) 
    et = D * ET_par.beta*A/pow(D, ET_par.kappa) *
    pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
    fCO2mean +  // Mean effect of CO2 on transpiration
    ET_par.chi *  s / (s + psychom) * (1-fAPAR) *  fWsub * ppfd;
    
    if (etmodel == 1) 
    et = D * ET_par.beta*A/pow(D, ET_par.kappa) *
    pow(fWgpp, ET_par.nu) * 
    fCO2mean +  
    ET_par.chi * (1-fAPAR) *  fWsub * ppfd;
    
    if (etmodel == 2) 
    et = D * (1 + ET_par.beta/pow(D, ET_par.kappa)) * A / CO2 * 
    pow(fWgpp, ET_par.nu) *
    fCO2mean +  
    ET_par.chi * (1-fAPAR) *  fWsub * ppfd;
    
  }

  \item{returncols}{What columns are returned, defaults to
    c('GPP','ET','SW'). Other possible options are fD for vapour
    pressure deficit modifier, fW for soil water modifier, fE (minimum
    of fD, fW), fS (temperature/season modifier), SOG (snow on ground,
    mm weq.),  Throughfall, Interception, Snowmelt (mm per day),
    Drainage, i.e. water melted or precipitated that is above field capacity runs off with a small delay (see parameter tau), Canopywater for surfacial water storage (upper limit set by parameter Cmax and fAPAR), S season status (C) for fS calculation. }
}


\seealso{
  Package information: \code{\link{Rpreles}}
}

\examples{
## Run model with artificial inputs
CO2 <- 280:(2*380)
T=rep(18, length(CO2))
fAPAR=rep(1, length(CO2))
Precip=rep(3, length(CO2))
PAR=rep(20, length(CO2))

## Plot CO2 effect on GPP, ET, and SW. Feedbacks through soil
## eliminated with high precipitation
pdf('testCO2.pdf', hei=10, wid=10)
op <- par(mfrow=c(4,4), mar=c(1,1,1,1), oma=c(4,4,4,4))
for (D in c(0, 0.5, 1, 1.5)) {
    D <- rep(D, length(CO2))
    o1 <- PRELES(PAR, T, D, Precip, CO2, fAPAR,
  returncols=c("GPP", "ET", "SW", "fW", "fE"), LOGFLAG=0)
    plot(CO2, o1$GPP)
abline(v=380)
    plot(CO2, o1$ET)
abline(v=380)
    plot(CO2, o1$GPP/o1$ET)
abline(v=380)
    plot(CO2, o1$SW)
abline(v=380)
}
}
