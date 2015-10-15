#include "prelesglobals.h"
/* 26.4.2013 Mikko Peltoniemi, Implemented PRELES model to R */
/* 21.6.2012. Mikko Peltoniemi, mikko.peltoniemi@metla.fi
 * Citations: Peltoniemi et al., manuscript submitted.
 *            Contact Mikko for the current status of the manuscript.
 *
 * Motivation for this model has been to prepare a simple model
 * for the prediction of drought effect on GPP that could be
 * run at high resolution for broad spatial scales.
 *
 * Model has been fitted using MCMC to Hyytiälä data, with embedded MCMC code 
 * of J. Rosenthal (borrowed from amcmc package). Following parameter estimates
 * were obtained (highest LL set)

RUN_PARAMETERS_FOR_MODEL
0 useMeasurement
0 LOGFLAG
SITE_SPECIFIC_PARAMETERS
413.0 soildepth
0.450 ThetaFC
0.118 ThetaPWP
3 tauDrainage
GPP_MODEL_PARAMETERS
0.748464 betaGPP
12.74915 tauGPP
-3.566967 S0GPP
18.4513 SmaxGPP
-0.136732 kappaGPP
0.033942 gammaGPP
0.448975 soilthresGPP
2000 cmCO2
0.4 ckappaCO2
EVAPOTRANSPIRATION_PARAMETERS
0.33271 betaET
0.857291 kappaET
0.041781 chiET
0.474173 soilthresET
0.278332 nuET
SNOW_RAIN_PARAMETERS
1.5 Meltcoef
0.33 I_0
4.824704 CWmax
57, ## t0 fPheno_start_date_Tsum_accumulation; -999 for conif
1.5, ## tcrit, fPheno_start_date_Tsum_Tthreshold, -999 for conif
134 ##tsumcrit, fPheno_budburst_Tsum, -999 for conif
 *
 * 
 *
 *
*/


/* R interface function, replaces main() */
void call_preles(// INPUTS
		 double *PAR, double *TAir, double *VPD, double *Precip, double *CO2, double *fAPAR,  
     double *GPPmeas, double *ETmeas, double *SWmeas,
                 // OUTPUTS
                 double *GPP, double *ET, double *SW, double *SOG,
                 double *fS, double *fD, double *fW,  double *fE,
		 double *Throughfall, double *Interception, double *Snowmelt,
		 double *Drainage,
		 double *Canopywater, double *S,
  
		 //PARAMETERS
		 double *soildepth, 
		 double *ThetaFC, 
		 double *ThetaPWP, 
		 double *tauDrainage, 
		 double *beta, // START GPP PARAMETERS
		 double *tau, 
		 double *S0,
		 double *Smax,
		 double *kappa,
		 double *gamma,
		 double *soilthres, // used for fW with ETmodel = 2 | 4 | 6
		 double *bCO2, // used for fW with ETmodel = 1 | 3 | 5
		 double *xCO2, // used for fW with ETmodel = 1 | 3 | 5)  ;
		 double *ETbeta, // START ET PARAMETERS
		 double *ETkappa, 
		 double *ETchi,
		 double *ETsoilthres, // used for fW with ETmodel = 2 | 4
		 double *ETnu, // used for fW with ETmodel = 1 | 3 
		 double *MeltCoef, // START WATER/SNOW PARAMETERS
		 double *I0, 
		 double *CWmax,
		 double *SnowThreshold,
		 double *T_0, 
		 double *SWinit, // START INITIALISATION PARAMETERS // Soilw water at beginning
		 double *CWinit, // Canopy water
		 double *SOGinit, // Snow on Ground 
		 double *Sinit, // State of temperature acclimation
		 double *t0,
		 double *tcrit,
		 double *tsumcrit,
		 int *etmodel, int *LOGFLAG, int *NofDays, 
		 int *day, 
		 double *transp, 
		 double *evap, 
		 double *fWE) {
    
  extern int preles(int NofDays, double *PAR, double *TAir, double *VPD, double *Precip,
		    double *CO2,
		    double *fAPAR,  p1 Site_par, p2 GPP_par, p3 ET_par,p4 SnowRain_par,
		    int etmodel ,
		    double *GPP, double *ET, double *SW, double *SOG,
		    double *fS, double *fD, double *fW,  double *fE,
		    double *Throughfall, double *Interception, double *Snowmelt,
		    double *Drainage,
		    double *Canopywater,
		    double *GPPmeas, double *ETmeas, double *SWmeas, double *S,
		    int LOGFLAG, long int multisiteNday, 
		    int *day, 
		    double *transp, 
		    double *evap, double *fWE)  ;      

  /* Parameter structs */
  p1 parSite;
  p2 parGPP;
  p3 parET;
  p4 parSnowRain;
  
  /* Read in model parameters */
  parSite.soildepth = *soildepth;
  parSite.ThetaFC = *ThetaFC;
  parSite.ThetaPWP = *ThetaPWP;
  parSite.tauDrainage = *tauDrainage;
  parGPP.beta = *beta; 
  parGPP.tau = *tau;
  parGPP.S0 = *S0;
  parGPP.Smax =*Smax;
  parGPP.kappa = *kappa;
  parGPP.gamma = *gamma;
  parGPP.soilthres = *soilthres;
  parGPP.bCO2 = *bCO2;
  parGPP.xCO2 = *xCO2;
  parGPP.t0 = *t0;
  parGPP.tcrit = *tcrit;
  parGPP.tsumcrit = *tsumcrit;
  parET.beta = *ETbeta;
  parET.kappa = *ETkappa;
  parET.chi = *ETchi;
  parET.soilthres = *ETsoilthres;
  parET.nu = *ETnu;
  parSnowRain.MeltCoef = *MeltCoef;
  parSnowRain.I0 = *I0; 
  parSnowRain.CWmax = *CWmax;
  
  parSnowRain.SnowThreshold=0;
  parSnowRain.T_0=0;
  parSnowRain.SnowThreshold=0;
  parSnowRain.T_0=0;
  
  // Forward init values (previous day values) as first values of result vectors
  SW[0] = *SWinit;
  Canopywater[0] = *CWinit;
  SOG[0] = *SOGinit;
  S[0] = *Sinit;

  FILE *flog=NULL;
  if (*LOGFLAG > 0.5) {
    flog = fopen("preles.log", "w"); // EXCEPTION LOGGING
    if (flog) {
      fprintf(flog, "call_preles(): First day weather:\nDOY=%d\tPPFD=%lf\tT=%lf\tVPD=%lf\tP=%lf\tCO2=%lf\n"
	      "fAPAR=%lf\tSW=%lf\tCW=%lf\tSOG=%lf\tS=%lf\n",
	      day[0], PAR[0], TAir[0], VPD[0], Precip[0],  CO2[0],fAPAR[0], 
	      SW[0], Canopywater[0], SOG[0], S[0]);
      if (*LOGFLAG > 1.5) 
	fprintf(flog, 
		"call_preles(): Parameters: N=%i\tparGGP.beta=%lf\tparET.chi=%lf\tparSnowRain.SnowThreshold=%lf\tetmodel=%d\nLOGFLAG=%d parGPP.t0=%lf\n",
		*NofDays, parGPP.beta, parET.chi, parSnowRain.SnowThreshold, *etmodel, 
		*LOGFLAG, parGPP.t0);
      
    }  else {
      //exit(1);
    }
    fclose(flog);
  }
  
  /* Call the workhorse function ------------------------------------------ */
  int notinf = 0;
  

  notinf = preles(*NofDays, PAR, TAir,
		  VPD, Precip,CO2,
		  fAPAR, parSite,
		  parGPP, parET, parSnowRain, *etmodel ,
		  GPP, ET, SW, SOG, fS, fD, fW,  fE,
		  Throughfall, Interception, Snowmelt,
		  Drainage, Canopywater,
		  GPPmeas, ETmeas, SWmeas, S, *LOGFLAG, *NofDays, day, 
		  transp, evap, fWE);
                                
  if (*LOGFLAG > 0.5) {
    flog = fopen("preles.log", "a"); // EXCEPTION LOGGING 



    if (flog) {
      fprintf(flog,  "call_preles(): preles() returned code %d...finishing\n", notinf);
      fclose(flog);
    } else {
      //exit(1);
    }           
  }


}
                                
