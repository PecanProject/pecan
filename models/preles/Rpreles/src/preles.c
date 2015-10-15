#include "prelesglobals.h"

/* Model function:
 *   1. Makes initialisations
 *   2. Estimates GPP
 *   3. Estimates snow and interception
 *   4. Estimates Evapotranspiration
 *   5. Updates soil water balance
*/ 

int preles(int NofDays,
	   double *PAR, double *TAir, double *VPD, double *Precip, 
	   double *CO2,
	   double *fAPAR,  p1 Site_par,
	   p2 GPP_par, p3 ET_par,  p4 SnowRain_par, int etmodel,
	   double *GPP, double *ET, double *SW, double *SOG,
	   double *fS, double *fD, double *fW,  double *fE,
	   double *Throughfall, double *Interception, double *Snowmelt,
	   double *Drainage, double *Canopywater,
	   double *GPPmeas, double *ETmeas, double *SWmeas, double *S, 	   
	   int LOGFLAG, long multisiteNday, int *day, 
	   double *transp, 
	   double *evap, double *fWE){

  extern double fS_model(double *S, double T, p2 GPP_par);
  extern double fPheno_model(p2 GPP_par, double T, double *PhenoS, int DOY);
    


  extern double ETfun(double D, double theta, double ppfd, double fAPAR, 
		      double T,
		      p3 ET_par, p1 Site_par,
		      double *canw,
		      double *fE, double A,
		      double fWgpp,  double fCO2mean, 
		      double CO2, 
		      FILE *flog, int LOGFLAG, int etmodel, 
		      double *transp, 
		      double *evap, double *fWE, double *fS);
  
  extern void  interceptionfun(double *rain, double *intercepted, double Temp, p4
			       SnowRain_par, double fAPAR);
  extern void swbalance(double *theta, double throughfall, 
			double snowmelt, double et,
			p1 sitepar, double *drainage,
			double *snow, double *canw, p4 SnowRain_par);
  extern void  Snow(double T, double *rain, double *snow, p4 SnowRain_par,
		    double *SnowMelt);
  extern void initConditions(double **PAR, double **TAir, 
			     double **VPD, double **Precip,
			     double **CO2);

  extern void GPPfun(double *gpp, double *gpp380, double ppfd,  
		     double D, double CO2, 
		     double theta, 
		     double fAPAR, double fSsub, 
		     p2 GPP_par, p1 Site_par, double *fD, double *fW,
	      double *fE, double *fECO2, FILE *flog, int LOGFLAG);
    
  extern double fCO2_VPD_exponent(double CO2, double xCO2 ) ;

  extern double fCO2_ET_model_mean(double CO2, p2 GPP_par );
  
  extern double fCO2_ET_VPD_correction(double fEgpp, double xCO2 );
  extern double fCO2_model_mean(double CO2, double bCO2 ) ;
  
  
  FILE *flog=NULL;
  flog = fopen("preles.log", "a"); // EXCEPTION LOGGING
  
  if (LOGFLAG > 0.5) fprintf(flog, "  Stepped into preles()");
  
  double I, T, D, P, theta, theta_snow, theta_canopy, S_state;
  double PhenoS=0;
  int fPheno=0;
  int i; 
  double  fEgpp, out=-999;
  double  fECO2gpp, gpp380;
  
  initConditions(&PAR, &TAir, &VPD, &Precip, &CO2);
  theta=SW[0];
  theta_canopy=Canopywater[0];
  theta_snow=SOG[0];
  S_state = S[0];
  
  
  if (LOGFLAG > 1.5) {
    fprintf(flog, "   preles(): Starting values for storage components:\nSW=%lf\tCW=%lf\tSOG=%lf\tS=%lf\n"
	    "   ...will loop %d rows of weather input\n",
	    theta, theta_canopy, theta_snow, S[0], NofDays);
    printf("   preles(): Site fAPAR =%lf,  LUE = %lf and soil depth = %lf.\n",
	   fAPAR[0], GPP_par.beta, Site_par.soildepth);
  }
  //    fclose(flog);
  

  
  

  
  /* ---------------------------------------------------------------------*/
  /* START LOOPING DAYS---------------------------------------------------*/
  /* ---------------- ----------------------------------------------------*/
  for (i=0; i < NofDays; i++) { 
    
    if ((LOGFLAG > 1.5)) {
      fprintf(flog, "   \ni=%d/%d,\t SW=%lf\tCW=%lf\tSOG=%lf\tS=%lf\n",
	      i+1, NofDays, theta, theta_canopy, theta_snow, S_state);
    }      
    
    /* Use previous day environment for prediction, if current values are missing,
       or suspicious*/
    if (i > 0) { 
	if (PAR[i] < -900) PAR[i] = PAR[i-1];
	if (TAir[i] < -900) TAir[i] = TAir[i-1];
	if (VPD[i] < 0 || VPD[i] > 6) VPD[i] = VPD[i-1];
	if (Precip[i] <    0) Precip[i] = Precip[i-1] * 0.3; 
	/* On avg. P+1=0.315*P 
	 * (in Sodis & Hyde) */
	if (CO2[i] < 0) CO2[i] = CO2[i-1];
	if (GPPmeas[i] < -990) GPPmeas[i] = GPPmeas[i-1]; 
	if (ETmeas[i] < -990) ETmeas[i] = ETmeas[i-1];    
	if (SWmeas[i] < 0.0) SWmeas[i] = SWmeas[i-1];   
	if (SW[i] < -900) SW[i] = SW[i-1]; 
	if (SOG[i] < -900) SOG[i] = SOG[i-1]; // See above, could be used for 
	// calibration	
    }
    
    
    if ((LOGFLAG > 1.5)) {
      fprintf(flog, "   weather inputs: PAR=%lf\tT=%lf\tVPD=%lf\tP=%lf\tCO2=%lf\n",
	      PAR[i], TAir[i], VPD[i], Precip[i], CO2[i]);
    }   
    
    /* Assign current values for environment */
    I = PAR[i]; T=TAir[i]; D=VPD[i]; P=Precip[i]; 
    /* Update temperature state that tells about seasonality -
     * for GPP and through GPP to ET */
    fS[i] = fS_model(&S_state, T, GPP_par);

    if (LOGFLAG > 1.5) fprintf(flog, "   preles(): estimated fS=%lf\n", fS[i]);
    
    
    /* Deciduous phenology - don't use if this information is inputted in fAPAR */
    /* Note also that fapar is multiplied by 0 or 1 (i.e. leaf development is not accounted for) */
    /* Model predicts budbreak based on critical threshold temperature sum */ 
    /* Note that this implementation works only if data starts before t0-date of fPheno-model */      
    if (LOGFLAG > 1.5) fprintf(flog, 
			       "   preles(): stepping into fPheno_model: inputs:\n      GPP_par.t0=%lf\tT=%lf\tPhenoS=%lf\tDOY=%d\n", 
			       GPP_par.t0, T, PhenoS, day[i]);

    fPheno = fPheno_model(GPP_par, T, &PhenoS, day[i]);
    if (LOGFLAG > 1.5) fprintf(flog, "   preles(): PhenoS=%lf\tfPheno=%d\n", 
			       PhenoS, fPheno );

    fAPAR[i] = fAPAR[i] * fPheno; 
    if (LOGFLAG > 1.5) fprintf(flog, 
			       "   preles(): fAPAR changed to %lf\n", 
			       fAPAR[i]);
    

    GPPfun(&GPP[i], &gpp380, I, D, CO2[i], theta, fAPAR[i], fS[i],
		    GPP_par, Site_par,  &fD[i], &fW[i], &fEgpp, &fECO2gpp, 
		    flog, LOGFLAG );

    if (LOGFLAG > 1.5) 
      fprintf(flog, 
	      "   preles(): estimated GPP=%lf\tfD=%lf\tfEgpp=%lf\tfECO2gpp=%lf\n   at 380ppm GPP=%lf\n", 
	      GPP[i], fD[i], fEgpp, fECO2gpp, gpp380);
    
    
    /* Calculate amount of snow and snowmelt at the end of the day */
    Snow(T, &P, &theta_snow, SnowRain_par, &Snowmelt[i]);
    
    // NOTE: interception model could be better
    Throughfall[i]  = P; 
    interceptionfun(&Throughfall[i], &Interception[i], T, 
		    SnowRain_par, fAPAR[i]);
    
    if (LOGFLAG > 1.5) 
      fprintf(flog, 
	      "   preles(): estimated Thr.fall=%lf\tIntercept.=%lf\tSOG=%lf\tSnowmelt=%lf\n", 
	      Throughfall[i], Interception[i], SOG[i], Snowmelt[i]);
    
    /*Excess water from canopy will drip down to soil if not evaporated 
      during the day, rest remains in canopy for the next day*/
    
    if (SnowRain_par.CWmax <= 0.00000001) { 
      Throughfall[i] = Throughfall[i] + Interception[i];
    } else {
      if (Interception[i] + theta_canopy > SnowRain_par.CWmax * fAPAR[i]) {
	Throughfall[i] = Throughfall[i] + Interception[i] + 
	  theta_canopy - SnowRain_par.CWmax  * fAPAR[i];
	theta_canopy = SnowRain_par.CWmax  * fAPAR[i];	
      } else {
	theta_canopy = Interception[i] + theta_canopy;
      }     
    }
    
    if (LOGFLAG > 1.5) 
      fprintf(flog, "   preles(): estimated canopy water=%lf\n", 
	      theta_canopy);
    
    
    ET[i] = ETfun(D, theta, I, fAPAR[i], T, 
		  ET_par, Site_par,
		  &theta_canopy,
		  &fE[i], // Soil water constrain on evaporation  
		  gpp380, 
		  fW[i], // soil water constrain of GPP at 380 ppm
		  fCO2_ET_model_mean(CO2[i], GPP_par),
		  CO2[i], 
		  flog, LOGFLAG, etmodel, 
		  &transp[i], 
		  &evap[i], &fWE[i], &fS[i]);                     
    
    if (LOGFLAG > 1.5) 
      fprintf(flog, 
	      "   preles(): ET=%lf\n", ET[i]);

     
      /* Calculate soil water balance, drainage and related variables at the 
         end of the day, as well as update snow and canopy water with et */
    
    //    swbalance(&theta, Throughfall[i], Snowmelt[i], ET[i],
    swbalance(&theta, Throughfall[i], Snowmelt[i], ET[i],
	      Site_par, &Drainage[i], //&Psi[i], &Ks[i], 
	      &theta_snow, &theta_canopy, SnowRain_par);

    if (LOGFLAG > 1.5) 
      fprintf(flog, 
	      "   preles(): drainage=%lf, after ET: SW=%lf\tSOG=%lf\tCW=%lf\n", 
	      Drainage[i], theta, theta_snow, theta_canopy);
    
    /* Record result variables with storage components */
    SOG[i] = theta_snow;
    SW[i] = theta;
    Canopywater[i] = theta_canopy;
    S[i]=S_state;
    
    if (LOGFLAG > 1.5) 
      fprintf(flog, 
	      "   preles(): after day state:\n   SW=%lf\tCW=%lf\tSOG=%lf\tS=%lf\n\n", SW[i], Canopywater[i], SOG[i], S[i]);

    
  } // END DAY LOOP
  

  if (LOGFLAG > 1.5) 
    fprintf(flog, 
	    "   preles(): looped all days, closing preles.log, exiting...\n");
  if (flog) fclose(flog);
  return(0);
}
    
