#include "prelesglobals.h"

/* Replace missing first day values with something reasoble if missing */
void initConditions(double **PAR, double **TAir, double **VPD, double **Precip, 
                    double **CO2) {
  /* if first day value is missing assume we're somewhere on the boreal
   * zone (lat > 60 deg) */
  if ( **PAR < -900) **PAR=5; // it was a dark winter day...
  if ( **TAir < -900) **TAir=0;
  if ( **VPD < 0 || **VPD > 6) **VPD=0.5; // VPD > 6 implausible, 3 = very dry air
  if ( **Precip < 0) **Precip=0;
  if ( **CO2 < 0.1) **CO2=380; //

};
