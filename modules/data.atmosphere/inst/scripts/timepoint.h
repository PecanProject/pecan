
#ifndef LSMTIMEPOINT
#define LSMTIMEPOINT

#include <strstream>

class LSMtimepoint{
  public:
  int year; 
  float decday, geoht, u, v, theta, rv, pi0, dn0, conprr, pcpg, sclp, rlong, rshort;  //*.dat values
  float rdiff;  //*-rad.dat values
  float month,lai,Babove,FSW,npp,nep,cflux,cco2,transp,evap,ch20,Wflux,Hflux,sensgc,sensvc,Tcanopy,fsc,stsc,soilW,soilT,snowD,snowT,Lresp,Rresp,Gresp,runoff,albedo,Rh,gpp;  //semi-monthly summary values
  LSMtimepoint();
};

std::ostream & operator<<(std::ostream &, LSMtimepoint);

#endif
