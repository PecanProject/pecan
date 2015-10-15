
#ifndef PRELESGLOBALS_H
#define	PRELESGLOBALS_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>
#include <string.h>

#define TARGACCEPT 0.44
#define MAXK 1000
#define MYINFINITY 999999999.9
#define PI 3.1415926535
#define NUMBER_OF_MODEL_PARAMETERS 38

int K;
int vectorlength;

/* Site soil and other parameters, some in input, some calculated in code */
typedef struct p1 {  
  double soildepth; 
  double ThetaFC;  
  double ThetaPWP;  
  double tauDrainage;
} p1 ;

//  GPP model 
typedef struct p2 {
  double beta; 
  double tau; 
  double S0;
  double Smax;
  double kappa;
  double gamma;
  double soilthres; // used for fW with ETmodel = 2 | 4 | 6
  double bCO2; // used for fW with ETmodel = 1 | 3 | 5
  double xCO2; // used for fW with ETmodel = 1 | 3 | 5;
  double t0; // Birch phenology parameters: 26th Feb = 57 DOY
  double tcrit; // (Linkosalo et al)           1.5 C
  double tsumcrit; //                                 134  C
  
} p2 ;

// ET-model
typedef struct p3 {
  double beta; 
  double kappa; 
  double chi;
  double soilthres; // used for fW with ETmodel = 2 | 4
  double nu; 
} p3 ;

// Rain and Snow models: interception and melting of snow 
typedef struct p4 {
  double MeltCoef; 
 // double Ifrac;
  double I0; 
  double CWmax;
  double SnowThreshold;
  double T_0;
} p4; 

// Storage components
typedef struct p5 {
  double SW; // Soilw water at beginning
  double CW; // Canopy water
  double SOG; // Snow on Ground 
  double S; // State of temperature acclimation
} p5; 

typedef struct p6 {
  double cvGPP; // Coefficients of variation for GPP, ET and SW
  double cvET;  // Used in MCMC-calibration only
  double cvSW; 
} p6; 


#ifdef	__cplusplus
extern "C" {
#endif


#ifdef	__cplusplus
}
#endif

#endif	/* PRELESGLOBALS_H */

