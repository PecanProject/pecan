#include "prelesglobals.h"

/* Seasonality model of Mäkelä et al 2004 */
double fS_model(double *S, double T, p2 GPP_par) {
  double fS; 
  
  *S = *S + (T-*S)/GPP_par.tau;
  if (0 > *S-GPP_par.S0) fS=0; else fS= *S-GPP_par.S0;
  if (1 < fS/GPP_par.Smax) fS=1; else fS=fS/GPP_par.Smax;
  
  return(fS);
};


double fPheno_model(p2 GPP_par, double T, double *PhenoS, int DOY) {
  double m; 
  int fPheno=0;

  if (GPP_par.t0 > -998) { // ie not -999 
    
    if ( (DOY > (GPP_par.t0 - 0.5)) & (DOY < 364) )  {
      m = (T - GPP_par.tcrit);
      if (m < 0) m = 0;
      *PhenoS = *PhenoS + m ;      
    } else {
      *PhenoS = 0;
    }

    if (*PhenoS > GPP_par.tsumcrit - 0.005) fPheno = 1; else fPheno = 0;


  } else {
    fPheno = 1;
  }

  return(fPheno);
};
/* *****************************************************************/
/* f-modifiers for increasing CO2 prepared by P. Kolari, pers. comm.*/
/*double fCO2_model_mean(double CO2, p2 GPP_par ) {
  return(1 + (CO2-380)/(CO2-380+GPP_par.bCO2));
}
double fCO2_VPD_exponent(double CO2, p2 GPP_par ) {
  return(pow(CO2/380, GPP_par.xCO2));
}
*/
double fCO2_model_mean(double CO2, double b ) {
  return(1 + (CO2-380)/(CO2-380+b));
}
double fCO2_VPD_exponent(double CO2, double xCO2 ) {
  return(pow(380/CO2, xCO2));
}

/* Note: 'ET_par.bC02' is the same as GPP_par.bCO2 */
double fCO2_ET_model_mean(double CO2, p2 GPP_par ) {
  return(1 - 1.95*(CO2-380)/(CO2-380+(GPP_par.bCO2)));
}
/* *****************************************************************/


/* GPP model, modified from Mäkelä et al 2008 */
double GPPfun(double *gpp, double *gpp380, 
	      double ppfd,  double D, double CO2, double theta, 
	      double fAPAR, double fSsub, 
              p2 GPP_par, p1 Site_par, double *fD, double *fW,
	      double *fE, double *fECO2, FILE *flog, int LOGFLAG) {

    extern double fCO2_model_mean(double CO2, double b ) ;
    extern double fCO2_VPD_exponent(double CO2, double xCO2 ) ;
    double thetavol = theta/Site_par.soildepth;
    double REW=(thetavol-Site_par.ThetaPWP)/
        (Site_par.ThetaFC-Site_par.ThetaPWP);
    double fEsub, fWsub, fLsub, fDsub,  fECO2sub, fDCO2sub, fWCO2sub;


    /* Calculate first the reference condition (ca=380 ppm) effect */
    fDsub = exp(GPP_par.kappa * D);
    fDsub = fDsub > 1 ? 1 : fDsub;



    if (GPP_par.soilthres < -998) { 
      fWsub = 1.0;      /* e.g. -999 means no water control of GPP*/
    } else {
      if (REW < GPP_par.soilthres) {
        if (REW > 0.01) fWsub = REW/GPP_par.soilthres; else fWsub = 0.0;
      } else {
        fWsub = 1.0;
      }
    }

    fLsub = 1 / (GPP_par.gamma * ppfd + 1);

    if (fDsub > fWsub) fEsub=fWsub; else fEsub = fDsub;
    *fW = fWsub;
    *fD = fEsub;

    *gpp380 = GPP_par.beta *  ppfd *  fAPAR * fSsub * fLsub * fEsub;
    if (LOGFLAG > 1.5) 
      fprintf(flog, 
	      "   gpp(): Modifier values for GPP at 380ppm\n      fD=%lf\tfW=%lf\tfE=%lf\n   ...would lead to GPP380=%lf\n", 
	      fDsub, fWsub, fEsub, *gpp380);

    

    /* CO2 effect not only influences fD but also fW, due to stomatal action */
    
    fDCO2sub = fDsub * pow(exp(-0.4 * D),
			   fCO2_VPD_exponent(CO2, GPP_par.xCO2)) / exp(-0.4 * D) ;
    fWCO2sub = fWsub * pow(fWsub, fCO2_VPD_exponent(CO2, GPP_par.xCO2));

    
    if (LOGFLAG > 1.5) 
      fprintf(flog, 
	      "   gpp(): Modifier values for GPP at %lf\n      fD=%lf\tfW=%lf\tfCO2mean=%lf\n", 
	      CO2, fDCO2sub, fWCO2sub, fCO2_model_mean(CO2, GPP_par.bCO2));
    

    if (fDCO2sub > fWCO2sub) fECO2sub=fWCO2sub; else fECO2sub = fDCO2sub;
    //    *fD = fDCO2sub;
    //    *fW = fWCO2sub;
    *fECO2 = fECO2sub;


    *gpp = GPP_par.beta *  ppfd *  fAPAR * fSsub * fLsub  * fECO2sub * 
      fCO2_model_mean(CO2, GPP_par.bCO2) ;

    

}
