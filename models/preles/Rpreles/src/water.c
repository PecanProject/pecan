#include "prelesglobals.h"

/* Estimate Evapotranspiration according to a simple empirical model
 * that uses GPP prediction to calculate transpiration, as driven
 * by VPD. Evaporation is estimated with PPFD, which is a surrogate
 * for Rnet */
double ETfun(double D, double theta, double ppfd, double fAPAR, double T, 
             p3 ET_par, p1 Site_par,
             double *canw,
             double *fE, double A,
             double fWgpp,  double fCO2mean, double CO2, 
	     FILE *flog, int LOGFLAG, int etmodel, double *transp, 
	     double *evap, double *fWE, double *fS) {

    
  double pow();
  double thetavol = theta/Site_par.soildepth; 
  double REW=(thetavol-Site_par.ThetaPWP)/
    (Site_par.ThetaFC-Site_par.ThetaPWP);
  double fEsub = -999; /* Minimum of fW and fD returned if ET-model 
			* flag indicates similar modifier as for GPP */
  double fWsub=1, fDsub=1;
  double  et; 


  double lambda, psychom, s, rho;
  double cp = 1003.5; // J/(kg K) (nearly constant, this is dry air on sea level)
  double MWratio = 0.622; // Ratio of molecular wiegths of water vapor and dry air;
  double R = 287.058; // J/(kg K) Specific gas constant for dry air, wiki
  double zh, zm, d, zom, zoh;
    /*If pressure is not inputted use default */
  double pressure = 101300; // Pa  
  rho=pressure/(R * (T+273.15) ); // Dry air density, kg/m3
  lambda = (-0.0000614342 * pow(T, 3) + 0.00158927 * pow(T, 2) - 
	    2.36418 * T +  2500.79) * 1000; // J/kg
  psychom= cp * pressure / (lambda* MWratio); // Pa/C, wiki
  s = 1000 * 4098.0 * (0.6109 * exp((17.27 * T)/(T+237.3))) / 
    pow(T+237.3, 2);  // Pa/C! (Ice has nearly the same slope)



  /* Calculate soil constraint, simple linear following Granier 1987*/
  if (ET_par.soilthres < -998) { /*-999 omits water control*/
    fWsub = 1; 
  } else {
    if (REW < ET_par.soilthres) {
      if (REW > 0.01) fWsub = REW/ET_par.soilthres; else fWsub = 0.0;
    } else {
      fWsub = 1.0;
    }
  }

  
  /* If there is any water in canopy, evaporation is not reduced by
   * low soil water */
  if (*canw > 0.00000001) fWsub = 1;

  //  if (fDsub > fWsub) fEsub=fWsub; else fEsub = fDsub;     
  
  *fE = fWsub;   
  *fWE = fWsub;

  if (D < 0.01) D=0.01;

  if (LOGFLAG > 1.5)
    fprintf(flog, "   ETfun(): CO2mean=%lf\tat CO2=%lf\n", 
	    fCO2mean, CO2);
  
  
  if (etmodel == 0) {
    *transp = D * ET_par.beta*A/pow(D, ET_par.kappa) *
      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
      fCO2mean;
    *evap = ET_par.chi * s / (s + psychom) * (1-fAPAR) *  fWsub * ppfd;
    et = D * ET_par.beta*A/pow(D, ET_par.kappa) *
      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
      fCO2mean +  // Mean effect of CO2 on transpiration
      ET_par.chi * s / (s + psychom) * (1-fAPAR) *  fWsub * ppfd;
  }
/*  if (etmodel == 1) {*/
/*    et = D * ET_par.beta*A/pow(D, ET_par.kappa) **/
/*      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP*/
/*      fCO2mean +  // Mean effect of CO2 on transpiration*/
/*      ET_par.chi * (1-fAPAR) *  fWsub * ppfd;*/
/*  }*/
  if (etmodel == 1) {
    *transp = D * ET_par.beta*A/pow(D, ET_par.kappa) *
      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
      fCO2mean;
    *evap = s / (s + psychom) * (1-fAPAR * *fS) *  fWsub * pow(ppfd,ET_par.chi);
    et = *transp + *evap;/*D * ET_par.beta*A/pow(D, ET_par.kappa) *
      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
      fCO2mean +  // Mean effect of CO2 on transpiration
      100 * D + ET_par.chi * s / (s + psychom) * (fAPAR) *  fWsub * ppfd;*/
  }
/*  if (etmodel == 2) {*/
/*    et = D * (1 + ET_par.beta/pow(D, ET_par.kappa)) * A / CO2 * */
/*      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP*/
/*      fCO2mean +  // Mean effect of CO2 on transpiration      */
/*      ET_par.chi * (1-fAPAR) *  fWsub * ppfd;*/
/*  }*/
  if (etmodel == 2) {
    *transp = D * ET_par.beta*A/pow(D, ET_par.kappa) *
      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
      fCO2mean;
    *evap = (1-fAPAR) *  fWsub * pow(ppfd,ET_par.chi);
    et = *transp + *evap;/*D * ET_par.beta*A/pow(D, ET_par.kappa) *
      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
      fCO2mean +  // Mean effect of CO2 on transpiration
      100 * D + ET_par.chi * s / (s + psychom) * (fAPAR) *  fWsub * ppfd;*/
  }

  if (etmodel == 3) {
    *transp = D * ET_par.beta*A/pow(D, ET_par.kappa) *
      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
      fCO2mean;
    *evap = s / (s + psychom) * (1-fAPAR) *  fWsub * pow(ppfd,ET_par.chi);
    et = *transp + *evap;/*D * ET_par.beta*A/pow(D, ET_par.kappa) *
      pow(fWgpp, ET_par.nu) * // ET differently sensitive to soil water than GPP
      fCO2mean +  // Mean effect of CO2 on transpiration
      100 * D + ET_par.chi * s / (s + psychom) * (fAPAR) *  fWsub * ppfd;*/
  }

  
  if (LOGFLAG > 2.5)
    fprintf(flog, "      ETfun(): Model=%d\nD\t%lf\nET_par.beta\t%lf\nA\t%lf\npow(D, ET_par.kappa)\t%lf\npow(fWgpp, ET_par.nu)\t%lf\nfWgpp\t%lf\nET_par.nu\t%lf\nfCO2mean\t%lf\nCO2\t%lf\nET_par.chi\t%lf\ns/(s+psychom)\t%lf\n1-fAPAR\t%lf\nfWsum\t%lf\nppfd\t%lf\n-->et\t%lf\n",	    
	    etmodel, D, ET_par.beta, A, pow(D, ET_par.kappa), 
	    pow(fWgpp, ET_par.nu), fWgpp, ET_par.nu,
	    fCO2mean, 
	    CO2,
	    ET_par.chi ,   s / (s + psychom), 1-fAPAR, fWsub,  ppfd, et);

  
  return(et);
}


/*Interception is a fraction of daily rainfall, fraction depending on fAPAR*/
void  interceptionfun(double *rain, double *intercepted, double Temp, p4 
		       SnowRain_par, double fAPAR) {
  if (Temp > SnowRain_par.SnowThreshold)  {
    *intercepted = *rain * (SnowRain_par.I0 * fAPAR / 0.75); 
    *rain = *rain - *intercepted;
  } else {
    *intercepted = 0;
  }
}




/* Soil water balance is update with snowmelt and canopy throughfall
 * and evapotranspiration. No drainage occurs below field capacity */
void swbalance(double *theta, double throughfall, double snowmelt, double et, 
               p1 sitepar, double *drainage,
	       double *snow, double *canw, p4 SnowRain_par) {
   double st0, etfromvegandsoil=0;

  /* Evaporate first from wet canopy and snow on ground */

  if (SnowRain_par.CWmax > 0.00000001) { 
    if ( (*canw + *snow - et) > 0 ) {             
      if ( (*canw - et) > 0 ) { 
	*canw = *canw -et;
	etfromvegandsoil = 0;
      } else if (*canw - et < 0) { // in this case, there's enough snow left
	*snow = *snow + *canw - et;
	*canw = 0;
	etfromvegandsoil = 0;
      }    
    } else {
      etfromvegandsoil = et - *canw - *snow;
      *canw=0.0;
      *snow = 0.0;
    }

  } else {  
    if ( (*snow - et) > 0 ) {             
      *snow = *snow - et;
      etfromvegandsoil = 0;
    } else if (*snow - et < 0) { // in this case, there's enough snow left
      etfromvegandsoil = et - *snow;
      *snow = 0;
    } else {
      *snow = 0.0;
    }
  }

  et = etfromvegandsoil;

  /* Water balance without drainage */
  st0 = *theta + throughfall + snowmelt  - et;
  if (st0 <= 0) st0 = 0.0001;

  /* Calculate what is left to drainage after partial balance update above: */
  if (sitepar.tauDrainage > 0) {  


    // Simple time delay drainage above FC:
    if (st0 > sitepar.ThetaFC * sitepar.soildepth) { 
      *drainage = (st0 - sitepar.ThetaFC * sitepar.soildepth) / 
	sitepar.tauDrainage;      
    } else {
      *drainage = 0;
    }
    *theta = st0 - *drainage;


    /* Include marginal drainage below FC.
     * This was needed for model calibration only, below FC drainage
     * was practically zero, but important for convergence */
    /*
if (st0 > sitepar.ThetaFC * sitepar.soildepth) {
      *drainage = (st0 - sitepar.ThetaFC * sitepar.soildepth) / 
	sitepar.tauDrainage;      
    }
    if (*drainage < (sitepar.ThetaFC * sitepar.soildepth - 
		     sitepar.ThetaPWP * sitepar.soildepth) / 
	10000) //pow(sitepar.tauDrainage, 5)) 
	*drainage = (sitepar.ThetaFC * sitepar.soildepth - 
		     sitepar.ThetaPWP * sitepar.soildepth) / 
	  10000; //pow(sitepar.tauDrainage, 5);
    
    if (st0 <= sitepar.ThetaFC * sitepar.soildepth && 
	st0 > sitepar.ThetaPWP * sitepar.soildepth) { 
      *drainage = (st0 - sitepar.ThetaPWP * sitepar.soildepth) / 
	10000; //pow(sitepar.tauDrainage, 5);      
      *theta = st0 - *drainage;
    }
  
    if (st0 <= sitepar.ThetaPWP * sitepar.soildepth) {
      *drainage = 0;
      *theta = st0;
    }
    *theta = st0 - *drainage;
    */
    //****************************************************** */
      

  } 
  
}


/* Rain is snow below T > 0 C, and snow melts above O C. */
void  Snow(double T, double *rain, double *snow, p4 SnowRain_par, 
	   double *SnowMelt) {
  double NewSnow; 
  if (T < SnowRain_par.SnowThreshold) {
    NewSnow=*rain; 
    *rain = 0; 
  } else {
    NewSnow=0;
  } 
  
  if (T > SnowRain_par.T_0) 
    *SnowMelt = SnowRain_par.MeltCoef*(T-SnowRain_par.T_0);  
  else *SnowMelt=0;
  
  if (*snow + NewSnow - *SnowMelt < 0) {
    *SnowMelt=NewSnow + *snow;
    *snow =0;
  } else {
    *snow = *snow + NewSnow - *SnowMelt;
  }
};  
