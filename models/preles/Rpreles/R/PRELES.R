PRELES = function(PAR, TAir, VPD, Precip, CO2, fAPAR, ## REQUIRED
    GPPmeas=NA, ETmeas=NA, SWmeas=NA, ## OPTIONAL FOR BYPASSING PREDICTION
    p = rep(NA, 30), ## PARAMETER VECTOR. NA parameters replaced with defaults.
    DOY=NA, ## Needed for deciduous phenology (and if radmodel != 0), otherwise assume simulation
    ## starting DOY=1, and continuing  all years having 365 days
    ## Irrelevant if fPheno-parameters are -999 (default, used for conifers)
    LOGFLAG = 0, control=0, # Control is the E model selection parameter. 
    parmodel=0, LAT=NA, PAR0=NA,# If PAR is missing, set parmodel > 0 (and give lat and the DOY as input) # PAR0 is latititude and DOY specific information for parmodel 11 and 12
    returncols=c('GPP','ET','SW','fWE','fW','evap','transp'))  {


    
    len = as.integer(length(TAir))
    if (is.na(GPPmeas)) GPPmeas = rep(-999, len)
    if (is.na(ETmeas)) ETmeas = rep(-999, len)
    if (is.na(SWmeas)) SWmeas = rep(-999, len)
    transp = evap = fWE = rep(-999, len)

    
    if (any(is.na(DOY))) {
        if (any(!is.na(p[28:30])))
            if (LOGFLAG > 0) print('warning: Some leaf phenology parameter(s) set but DOY not provided, starting from Jan 1 and assuming 365 d / year.')
        DOY = rep(1:365, ceiling(len/365))
        DOY = DOY[1:len]
        if (LOGFLAG > 0 & parmodel > 0)
            print('warning: you are using model estimated PAR, are you sure DOY is correct, as it was assumed you are starting from Jan 1 and there are 365 d / year')
    }
    
    
    if (parmodel == 1 | parmodel == 2) { ## Theoretical radiation modified by VPD
        stopifnot(!any(is.na(LAT)))
        PAR=dPAR(LAT=LAT,DOY=DOY,VPD=VPD, radmodel=parmodel)
    }


    if (parmodel == 11 | parmodel == 12) { ## Speed-up, requires calc of dPAR0()
        
        PAR=dPAR1(PAR0,VPD=VPD, radmodel=parmodel)

    }
    
    
    
    ## DEFAULT PARAMETERS
    ## SITE
    if (control == 0) ## Priestley-taylor version calibration
        defaults = c(413.0, ## 1 soildepth
    0.450, ## 2 ThetaFC
    0.118, ## 3 ThetaPWP
    3, ## 4 tauDrainage
    ## GPP_MODEL_PARAMETERS
    0.748018, ## 5 betaGPP
    13.23383, ## 6 tauGPP
    -3.9657867, ## 7 S0GPP
    18.76696, ## 8 SmaxGPP
    -0.130473, ## 9 kappaGPP
    0.034459, ## 10 gammaGPP
    0.450828, ## 11 soilthresGPP
    2000, ## 12 cmCO2
    0.4, ## 13 ckappaCO2
    ## EVAPOTRANSPIRATION_PARAMETERS
    0.324463, ## 14 betaET
    0.874151, ## 15 kappaET
    0.075601, ## 16 chiET
    0.541605, ## 17 soilthresET
    0.273584, ## 18 nu ET
    ## SNOW_RAIN_PARAMETERS
    1.2, ## 19 Meltcoef
    0.33, ## 20 I_0
    4.970496, ## 21 CWmax, i.e. max canopy water
    0, ## 22 SnowThreshold, 
    0, ## 23 T_0, 
    200, ## 24 SWinit, ## START INITIALISATION PARAMETERS 
    0, ## 25 CWinit, ## Canopy water
    0, ## 26 SOGinit, ## Snow on Ground 
    20, ## 27 Sinit ##CWmax
   -999, ## t0 fPheno_start_date_Tsum_accumulation; conif -999, for birch 57
   -999, ## tcrit, fPheno_start_date_Tsum_Tthreshold, 1.5 birch
    -999 ##tsumcrit, fPheno_budburst_Tsum, 134 birch
            )
    if (control == 1) ## Peltoniemi et al., BER submitted
        defaults = c(413.0, ## 1 soildepth
            0.450, 0.118, 3, 0.748464, 12.74915, -3.566967, 18.4513, -0.136732,
            0.033942, 0.448975, 2000, 0.4, 0.33271, 0.857291, 0.041781,
            0.474173, 0.278332, 1.5, 0.33, 4.824704, 0, 0, 180, 0, 0, 10,
            -999, -999, -999) 

    if (control == 2) ## Peltoniemi et al., BER submitted
        defaults = c(413.0, ## 1 soildepth
            0.450, 0.118, 3, 0.748464, 12.74915, -3.566967, 18.4513, -0.136732,
            0.033942, 0.448975, 2000, 0.4, 0.33271, 0.857291, 0.041781,
            0.474173, 0.278332, 1.5, 0.33, 4.824704, 0, 0, 180, 0, 0, 10,
            -999, -999, -999) 

    if (control == 3) ## Peltoniemi et al., BER submitted
        defaults = c(413.0, ## 1 soildepth
            0.450, 0.118, 3, 0.748464, 12.74915, -3.566967, 18.4513, -0.136732,
            0.033942, 0.448975, 2000, 0.4, 0.33271, 0.857291, 0.041781,
            0.474173, 0.278332, 1.5, 0.33, 4.824704, 0, 0, 180, 0, 0, 10,
            -999, -999, -999) 


    p[is.na(p)] = defaults[is.na(p)] ## Note: this may slow down a bit when looping MCMC
    
       .C('call_preles', 
          PAR=as.double(PAR), TAir=as.double(TAir), VPD=as.double(VPD),
          Precip=as.double(Precip), CO2=as.double(CO2), fAPAR=as.double(fAPAR),  
          GPPmeas=as.double(GPPmeas), ETmeas=as.double(ETmeas), SWmeas=as.double(SWmeas),
          ## OUTPUTS
          GPP = double(len), ET=double(len), SW=double(len), SOG=double(len),
          fS=double(len), fD=double(len), fW=double(len),  fE=double(len),
          Throughfall=double(len), Interception=double(len), Snowmelt=double(len),
          Drainage =double(len),
          Canopywater=double(len), S=double(len),

            ##PARAMETERS
            p1=as.double(p[1]), 
            p2=as.double(p[2]), 
            p3=as.double(p[3]), 
            p4=as.double(p[4]), 
            p5=as.double(p[5]), ## START GPP PARAMETERS
            p6=as.double(p[6]), 
            p7=as.double(p[7]),
            p8=as.double(p[8]),
            p9=as.double(p[9]),
            p10=as.double(p[10]),
            p11=as.double(p[11]), ## used for fW with ETmodel = 2 | 4 | 6
            p12=as.double(p[12]), ## used for fW with ETmodel = 1 | 3 | 5
            p13=as.double(p[13]), ## used for fW with ETmodel = 1 | 3 | 5)  ;
            p14=as.double(p[14]), ## START ET PARAMETERS
            p15=as.double(p[15]), 
            p16=as.double(p[16]), 
            p17=as.double(p[17]), ## used for fW with ETmodel = 2 | 4
            p18=as.double(p[18]), ## used for fW with ETmodel = 1 | 3 
            p19=as.double(p[19]), ## START WATER/SNOW PARAMETERS
            p20=as.double(p[20]), 
            p21=as.double(p[21]),
            p22=as.double(p[22]), 
            p23=as.double(p[23]), 
            p24=as.double(p[24]), ## START INITIALISATION PARAMETERS // Soilw water at beginning
            p25=as.double(p[25]), ## Canopy water
            p26=as.double(p[26]), ## Snow on Ground 
            p27=as.double(p[27]), ## State of temperature acclimation
          p28=as.double(p[28]), ## Canopy water
            p29=as.double(p[29]), ## Snow on Ground 
            p30=as.double(p[30]), ## State of temperature acclimation
          etmodel=as.integer(control), ## useMeasurement, int *LOGFLAG, int *multisiteNday, int *NofDays
            LOGFLAG=as.integer(LOGFLAG),
            len=as.integer(len),
          DOY=as.integer(DOY),
          transp=as.double(transp), evap=as.double(evap),
          fWE=as.double(fWE))[returncols]


}
