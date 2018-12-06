#include "timepoint.h"

static const float fNA = -9999.0;

LSMtimepoint::LSMtimepoint():year(0),decday (0.0),
			     geoht(0.0), u(0.0), v(0.0), theta(0.0), rv(0.0), pi0(0.0), dn0(0.0), conprr(0.0), pcpg(0.0), sclp(0.0), rlong(0.0), rshort(0.0)
{
rdiff = month = lai = Babove = FSW = npp = nep = cflux = cco2 = transp = evap = ch20 = Wflux = Hflux = sensgc = sensvc = Tcanopy = fsc = stsc = soilW = soilT = snowD = snowT = Lresp = Rresp = Gresp = runoff = albedo = Rh = gpp=fNA;  //semi-monthly summary values
}


std::ostream & operator<<(std::ostream &out, LSMtimepoint d){
  out<<d.geoht<<"\t"<<d.u<<"\t"<<d.v<<"\t"<<d.theta<<"\t"<<d.rv<<"\t"<<d.pi0<<"\t"<<d.dn0<<"\t"<<d.conprr<<"\t"<<d.pcpg<<"\t"<<d.sclp<<"\t"<<d.rlong<<"\t"<<d.rshort;
  return out;
}
