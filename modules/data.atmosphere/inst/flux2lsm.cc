// Code to convert FLUXNET tower data into metdrivers for single site runs
// M Dietze

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <stdlib.h>
#include <vector>
#include <math.h>
#include "util.h"
#include "timepoint.h"

using namespace std;

//define constants

const bool AmeriDefault = 0; //use ameriflux parser for "defined" sites

const int do_dat = 1;  // generate the *.dat met files for model input
const int do_rad = 1;  // generate the *-rad.dat radiation files for model input
const int do_flux = 0; // generate semi-monthly & annual summaries of flux to compare to output

const bool radheader = 0;
const bool metheader = 0;

const int progress_freq = 100;

const float dtncep_default = 4.0;
const float dt_default = 4.0;
const float dtrad_default = 96.0;

const float T_st = 373.15; //steam temperature (K)
const float e_st = 1013.25; //saturation vapor pressure at steam temp (hPa)
const float radians = 3.14159/180.0;
const float Kelvin = 273.15;  //Celsius->Kelvin
const int NA = -9999;
const float fNA = -9999.0;
const float C_PARCONV = 1.0/(0.45*4.6);
static const int n_semi_len = 30;

/******* "IS MISSING" list*********
0  - short wave (15min)
1  - long wave (15min)
2  - geoht
3  - u
4  - v
5  - theta
6  - rv
7  - pi0
8  - dn0
9  - conprr
10 - pcpg
11 - sclp
12 - rlong
13 - rshort
*/


//define functions

inline float rh2rv(float rh, float T){
  ///converts relative humidity to specific humidity
  ///input: rh = relative humidity (proportion, not %)
  ///input: T  = absolute temperature
  return rh*2.541e6*exp(-5415.0/T)*18/29;;
}

inline float SatVapPres(float T){
  ///estimates saturation vapor pressure (kPa)  Goff-Gratch 1946
  ///input: T = absolute temperature
  return 0.1*exp( -7.90298*(T_st/T-1) + 5.02808*log(T_st/T) - 1.3816e-7*(pow(10,(11.344*(1-T/T_st)))-1) + 8.1328e-3*(pow(10,(-3.49149*(T_st/T-1)))-1) + log(e_st));  
}

inline float exner(float pres){
  /// estimated exner function
  /// input: pres = air pressure (Bar)
  return 1004.0*pow(pres,(287.0/1004.0));
}

inline float AirDens(float pres, float T, float rv){
  /// estimate air density from pressure, temperature, and humidity
  /// input: pres = air pressure (pascals)
  /// input: T    = air temperature (Kelvin)
  /// input: rv   = humidity
  return pres/(287.0*T*(1.0+0.61*rv));
}

inline int day2month(float day,int year){
  int month = 0;
  while(day > 0){
    month++;
    switch(month){
    case 1:
    case 3:
    case 5:
    case 7:
    case 8:
    case 10:
    case 12:
      day -= 31;
      break;
    case 2:
      if((year-2000)%4 == 0){
	day -= 29;  // leap year
      } else {
	day -= 28;
      }
      break;
    default:
      day -= 30;
    }
  }
  return month;
}

int firstday(int mo,int yr){
  static const int ldoy[13] = {0,31,60,91,121,152,182,213,244,274,305,335,366};  
  static const int doy[13]  = {0,31,59,90,120,151,181,212,243,273,304,334,365};  
  if((yr-2000)%4 == 0){return ldoy[mo-1];}
  return doy[mo-1];
}

int nday(int mo,int yr){
  static const int lnday[12]  = {31,29,31,30,31,30,31,31,30,31,30,31};  
  static const int nday[12]   = {31,28,31,30,31,30,31,31,30,31,30,31};  
  if((yr-2000)%4 == 0){return lnday[mo-1];}
  return nday[mo-1];
}
int calcDecDay(int day, int month, int year){
  return day-1 + firstday(month,year);
}

double dirfrac(int jday,double time,double rshort,double lat){
  
  double dayangle,eccen,solartime,ket,declination,frac;
  
  dayangle=2.0*3.14159*(jday)/365.25;
  declination=0.006918-0.399912*cos(dayangle)+0.070257*sin(dayangle)-0.006758*cos(2.0*dayangle)+0.000907*sin(2.0*dayangle)-0.002697*cos(3.0*dayangle)+0.00148*sin(3.0*dayangle);  
  eccen=1.00011+0.034221*cos(dayangle)+0.00128*sin(dayangle)+0.000719*cos(2.0*dayangle)+0.000077*sin(2.0*dayangle);
  //  solartime=time*24.0+6.56-12.0;
  //  solartime=time/3600.0+6.56-12.0;
  solartime=time/3600.0-12.0-1.0;
  ket=1367.0*eccen*(cos(declination)*cos(lat/360.0*2.0*3.14159)*cos(15.0/360.0*2.0*3.14159*(solartime))+sin(declination)*sin((lat)/360.0*2.0*3.14159));
  frac=rshort/ket;
  if (frac > 0.9) frac=0.9;
  if (frac < 0.0) frac=0.0;
  //  cout<<"-> "<<frac<<" "<<dayangle<<" "<<solartime<<" "<<declination<<" "<<eccen<<" "<<ket<<" "<<rshort<<endl;
  // cout<<ket<<"\t"<<rshort<<endl;
  return frac;
}



//***********  MAIN ************//

int main(int argc, char* argv[]){

  if(argc < 2){
    cout<<"flux2lsm <sitename> (refname) (frequency)"<<endl;
    exit(0);
  }
  string folder(argv[1]);
  cout<<folder<<endl;
  ifstream in,in2;
  string file,prefix;
  int yr_start,yr_stop;

  double dt(dt_default);
  double dtrad(dtrad_default);
  double dtncep(dtncep_default);

  bool readstraight(0);  //set to 1 if there is no header
  bool amerifluxParser(AmeriDefault); //use the general parser for Ameriflux sites
  int amerifluxLevel(3);  //"level" of flux data to use
  string ncepPath("../reanalysis/ncep/");
  if(argc > 2){
    ncepPath = argv[2];
    if(!(ncepPath[ncepPath.length()-1] == 47)){ncepPath += "/";}
    //    readstraight=1;
  }

  if(argc > 3){
    dt = atof(argv[3]);
  }
  dt = 86400.0/dt;
  dtncep = 86400.0/dtncep;

  //variables
  bool foundFolder(0);
  vector<LSMtimepoint> data; //main data matrix
  int i,ismissing[14];       //list of data that needs to be filled
  for(i=0;i<14;i++){ismissing[i] = 1;}
  int n_data[14];
  int year_first,year_last;
  float day_first,day_last;
  float lat,lon,elev,tlag(0.0);
  float tstep(1);  //default timestep (days).  used for generating rad files
                //tower data is interpolated unless sequential non-NA values are > tstep apart
  float rh,pres,T,vpd,svp,par,w,wdir; //common meteorological measurements


  //***********  PARSE DATA ACCORDING TO SITE *************

  if(folder == "temp"){
    foundFolder = 1;
    int month_first,month_last;
    //used to extract a SOI from NCEP files
    cout<<"Enter Lat: ";
    cin>>lat;
    cout<<"Enter Lon: ";
    cin>>lon;
    cout<<"Enter start day: ";
    cin>>day_first;
    cout<<"Enter start month: ";
    cin>>month_first;
    cout<<"Enter start year: ";
    cin>>year_first;
    cout<<"Enter stop day: ";
    cin>>day_last;
    cout<<"Enter stop month: ";
    cin>>month_last;
    cout<<"Enter stop year: ";
    cin>>year_last;
    
    LSMtimepoint start,stop;
    start.year = year_first;
    start.decday = calcDecDay((int)day_first,month_first,year_first);
    stop.year =  year_last;
    stop.decday = calcDecDay((int)day_last,month_last,year_last);

    start.rshort = start.rdiff = fNA-1.0;
    stop.rshort = stop.rdiff = fNA-1.0;

    data.push_back(start);
    data.push_back(stop);
  }

  if(folder == "ameriflux"){
    cout<<"Enter data folder path"<<endl;
    cin>>file;
    cout<<"Enter data file prefix"<<endl;
    cin>>prefix;
    cout<<"Enter first year"<<endl;
    cin>>yr_start;
    cout<<"Enter last year"<<endl;
    cin>>yr_stop;
    cout<<"Enter hours off GMT"<<endl;
    cin>>tlag;
    tlag = tlag/24.0;
    cout<<"Enter postprocessing level (3 = Level 3, 4 = Level 4)"<<endl;
    cin>>amerifluxLevel;
    amerifluxParser = 1;
  }

  if(folder == "morgan_monroe"){
    file = folder;
    prefix = "USMMS";
    yr_start = 1999;
    yr_stop = 2005;
    tlag = -5.0/24.0;  //check!  IN appears to not observe daylights!
    amerifluxParser = 1;
  }

  if(folder == "willow"){ //Willow Creek, WI, ChEAS
    file = folder;
    prefix = "USWCr";
    yr_start = 1999;
    yr_stop = 2006;
    tlag = -6.0/24.0; 
    amerifluxParser = 1;
  }
  if(folder == "sylvania"){ //Sylvania Wilderness, MI, ChEAS
    file = folder;
    prefix = "USSyv";
    yr_start = 2001;
    yr_stop = 2006;
    lat = 46.2420170;
    lon = -89.3476500;

    tlag = -6.0/24.0;  //check! near border
    amerifluxParser = 1;
  }
  if(folder == "lcreek"){ //Lost Creek, WI, ChEAS
    file = folder;
    prefix = "USLos";
    yr_start = 2000;
    yr_stop = 2005;
    lat = 46.0826800;
    lon = -89.9791900;

    tlag = -6.0/24.0;  //check! near border
    amerifluxParser = 1;
  }

  if(folder == "umbs"){ //University of Michigan Biological Station
    file = folder;
    prefix = "USUMB";
    yr_start = 1999;
    yr_stop = 2003;
    tlag = -5.0/24.0;
    amerifluxParser = 1;
  }

  if(folder == "duke_pine"){ //Duke Forest Pine site, NC
    file = folder;
    prefix = "USDk3";
    yr_start = 2001;
    yr_stop = 2005;
    tlag = -5.0/24.0;
    amerifluxParser = 1;
  }
  if(folder == "howland"){ //Howland Forest, Maine
    file = folder;
    prefix = "USHo1";
    yr_start = 1996;
    yr_stop = 2004;
    tlag = -5.0/24.0;
    amerifluxParser = 1;
  }

  if(folder == "bartlett"){  //Bartlett Experimental Forest Flux Tower
    foundFolder=1;
    //coords
    lat  =  44.06464;
    lon  = -71.28808;
    elev = fNA;
    tstep= 1.0/24.0;  // set to slightly more than an hour to make sure we interpolate hourly data
    tlag = -5.0/24; // 5hrs behind UTC

    // NOTE: original file uses -999 as NA value, switched to -9999 in emacs

    //mark which met data is loaded
    ismissing[0] = ismissing[3] = ismissing[4] = ismissing[5] = ismissing[8] = ismissing[9] = ismissing[10]= ismissing[11]=ismissing[13] = 0;
    file="/n/Moorcroft_Lab/Users/mcd/inputs/fluxnet/bartlett/Bartlett_for_FLUXNET.csv";
    if(checkfileR(file.c_str())){
      in.open(file.c_str());
    } else {cerr<<file<<" not found"<<endl; abort();}
    //remove header
    string line;
    for(i=0;i<13;i++){  //large header has summary info
      getline(in,line);
    }
    
    //load data
    float x;
    bool notemp(0),firstpass(1);
    while(getline(in,line)){
      notemp=0;

      LSMtimepoint d;
      csv2tab(line);
      line = switchNA(line,"NA","-9999");
      istringstream lin(line.c_str());
      lin>>d.year;
      lin>>d.decday;
      //extract date
      d.decday -= tlag - 1.0;  //adjust day by 1.0 to make january 1 start at 0
      if(d.decday < 0.0){ d.year--; d.decday += firstday(13,d.year);}
      if(d.decday >firstday(13,d.year))  {d.decday -= firstday(13,d.year); d.year++;} 
      if(firstpass){year_first = d.year; day_first = d.decday;firstpass=0;}
      lin>>d.cflux; //FC = raw CO2 flux (umol/m2/s)
      lin>>d.sclp;  //CO2_top (ppm)
      lin>>x;       //ustar
      lin>>par;     //PAR_in = umol/ms/s
      if(par > fNA){
	d.rshort = par*C_PARCONV;
      } else {d.rshort = fNA;}
      lin>>T;       //TA = air temperature (C)
      if(T > fNA){
	T = T + Kelvin;
      }
      lin>>d.Wflux;
      lin>>rh;      //relative humidity
      if(rh>fNA&&T>fNA){
	d.rv = rh2rv(rh/100.0,T);
	if(d.rv > 1){
	  cout<<"ERROR IN HUMIDITY\t"<<rh<<"\t"<<d.rv<<"\t"<<T<<endl;
	  cout<<line<<endl;
	  exit(2);
	}
      } else {
	d.rv = fNA;
      }
      lin>>x;        //LE = Latent heat flux (W/m2)
      lin>>d.Hflux;  //H  = Sensible heat flux (W/m2)
      lin>>d.sensgc; //G1 = Soil heat flux (W/m2)
      lin>>d.pcpg;   //precip (mm)  -> time units
      if(d.pcpg > fNA){
	d.conprr = 0.0;
	d.pcpg = d.pcpg/1800;   //convert mm/30min -> kg/m2/s
      }else {
	d.conprr = fNA;
      }
      lin>>d.soilW;    // Soil moisture (%)       ####### DEPTH??  
      lin>>d.soilT;    // Soil temperature (C) 
      lin>>x;          // RR = Reflected Radiation (W/m2)
      lin>>x;          // RNET = Net Radiation (W/m2)
      lin>>x;          // RD = Diffuse Radiation (W/m2)
      lin>>x;          // APAR = Absorbed PAR (umol/m2/s)

      lin>>wdir;       // Wind direction
      lin>>w;          // Wind speed (scalar mean)
      if(w > fNA && wdir > fNA){
	d.u = w*cos(wdir*radians);
	d.v = w*sin(wdir*radians);
      } else { d.u = d.v = fNA;}

      lin>>pres;       // PA = air pressure (kPa)
      lin>>x;          // ZL = Atmospheric stability parameter
      if(pres>fNA){
	d.pi0 = exner(pres/100);
	if(T > fNA && d.rv > fNA){
	  d.dn0 = AirDens(pres*1000,T,d.rv);
	} else {
	  d.dn0 = fNA;
	}
      } else {
	d.pi0=d.dn0=fNA;
      }
      d.theta = T;
      
      data.push_back(d);
      year_last=d.year;
      day_last=d.decday;
    }
    in.close();
    cout<<"loaded"<<endl;
  }

  if(folder == "coweeta"){ //Coweeta LTER gradient plots -> not AmeriFlux
    foundFolder = 1;
    lat = 35.04;
    lon = 83.46;
    tstep = 1.0/24;
    //float tlag = -5.0/24.0;

    //mark which data is loaded (Air Temp,RH)

  }

  if(folder == "harvard_met"){  //HARVARD FOREST MET STATION
    foundFolder=1;
    //coords
    lat  = 42.533;
    lon  = -72.190;
    elev = 340;
    tstep= 1.1/24;  // set to slightly more than an hour to make sure we interpolate hourly data
    tlag = -5.0/24; // 5hrs behind UTC

    //mark which met data is loaded
    ismissing[0] = ismissing[1] = ismissing[3] = ismissing[4] = ismissing[5] = ismissing[6] = ismissing[7] = ismissing[8] =ismissing[9] = ismissing[10]= ismissing[13] = 0;
    file="/home/mcd/inputs/fluxnet/met/HF_met.01-07.txt";
    if(checkfileR(file.c_str())){
      in.open(file.c_str());
    } else {cerr<<file<<" not found"<<endl; abort();}
    //remove header
    string line;
    getline(in,line);
    getline(in,line);

    //load data
    float w,wdir,rh,T,x;
    int jday,hr,min;
    string date,c,sub;
    bool notemp(0),firstpass(1);

    //set start time explicitly
    year_first = 1998;
    day_first  = 1;
    firstpass  = 0;

    while(getline(in,line)){
      notemp=0;

      LSMtimepoint d;
      csv2tab(line);
      //line = switchNA(line,"-99","-9999");
      line = switchNA(line,"NA","-9999");
      //cout<<line<<endl;
      istringstream lin(line.c_str());
      lin>>date;
      lin>>jday;
      //extract date
      sub = date.substr(0,4);
      d.year = atoi(sub.c_str());
      sub = date.substr(11,2);
      hr = atoi(sub.c_str());
      sub = date.substr(14,2);
      min = atoi(sub.c_str());
      d.decday = jday-tlag+hr/24.0+min/1440.0;
      if(d.decday < 0.0){ d.year--; d.decday += firstday(13,d.year);}
      if(d.decday >firstday(13,d.year))  {d.decday -= firstday(13,d.year); d.year++;} 
      if(firstpass){year_first = d.year; day_first = d.decday;firstpass=0;}
      //cout<<d.year<<"\t"<<d.decday<<"\t"<<hr<<"\t"<<min<<endl;
      lin>>T; lin>>c;  //temperature
      if(T > fNA){
	T = T + Kelvin;
	//svp = SatVapPres(T);
      } else {
	notemp=1;
      }
      lin>>rh; lin>>c;  //relative humidity
      if(rh>fNA&&T>fNA){
	d.rv = rh2rv(rh/100.0,T);
	if(d.rv > 1){
	  cout<<"ERROR IN HUMIDITY\t"<<rh<<"\t"<<d.rv<<"\t"<<T<<endl;
	  cout<<line<<endl;
	  exit(2);
	}
      } else {
	d.rv = fNA;
      }
      lin>>x; lin>>c;  //dewpoint
      lin>>d.pcpg; lin>>c;   //precip
      if(d.pcpg > fNA){
	d.conprr = 0.0;
	if(d.year < 2005){
	  d.pcpg = d.pcpg/3600;  //convert mm/hr -> kg/m2/s
	}else{
	  d.pcpg = d.pcpg/900;   //convert mm/15min -> kg/m2/s
	} 
      }else {
	d.conprr = fNA;
      }
      //cout<<"ppt = "<<d.pcpg<<endl;
      lin>>d.rshort; lin>>c;  //global solar radiation (W/m2)
      lin>>x; lin>>c;  // PAR
      lin>>x; lin>>c;  // Net Radiation
      lin>>pres; lin>>c;  // Barometric Pressure (millibar)
      if(pres>fNA){
	d.pi0 = exner(pres/1000);
	if(T > fNA && d.rv > fNA){
	  d.dn0 = AirDens(pres*100,T,d.rv);
	} else {
	  d.dn0 = fNA;
	}
	//cout<<pres<<"\t"<<d.pi0<<"\t"<<d.dn0<<endl;
	//	exit(2);
      } else {
	d.pi0=d.dn0=fNA;
      }
      lin>>w; lin>>c;  // Wind speed (scalar mean)
      lin>>x; lin>>c;  // Wind speed (vector mean)
      lin>>wdir; lin>>c;  // Wind direction
      lin>>x; lin>>c;  // Wind direction sd
      lin>>x; lin>>c;  // gust speed
      lin>>d.soilW;    // soil moisture (10cm)

      if(w > fNA && wdir > fNA){
	d.u = w*cos(wdir*radians);
	d.v = w*sin(wdir*radians);
      } else { d.u = d.v = fNA;}
      d.theta = T;
      
      //      cout<<d.year<<"\t"<<d.decday<<"\t"<<date<<endl;
      data.push_back(d);
      year_last=d.year;
      day_last=d.decday;
      //exit(1);
    }
    in.close();
    cout<<"loaded"<<endl;
  }//end HARVARD FOREST MET STATION

  if(folder == "harvard_tsca"){  // HARVARD HEMLOCK STAND    ******** need to update for tstep, UTC, semimonthly values
    foundFolder = 1;
    //set coordinates
    lat = 42.53933;
    lon = -72.17794; 
    tstep = 1.0/24;
    //set time offset (difference from UTC)
    tlag = -5.0/24.0;
    //float tlag = 0.0;
 
    //set start time explicitly
    year_first = 1993;
    day_first  = 1;
    bool firstpass  = 0;


    //mark which data loaded
    ismissing[0] = ismissing[1] = ismissing[3] = ismissing[4] = ismissing[5] = ismissing[6] = ismissing[13] = 0;
    //set files
    file = folder+"/Harvard_For_hemlock_C_flux.csv";
    string file2 = folder+"/Harvard_Forest_Hemlock_2004.csv";
    string file3 = folder+"/HF_Hemlock.met05-06.csv";

    // *************  LOAD FIRST FILE (2001) ***************** //
    if(checkfileR(file.c_str())){
      in.open(file.c_str());
    } else {cerr<<file<<" not found"<<endl; abort();}
    //remove header
    string line;
    getline(in,line);
    getline(in,line);
    getline(in,line);

    //load data
    float x,ustar;
    bool notemp(0);
    while (getline(in,line)){
      notemp=0;

      LSMtimepoint d;
      //clean line
      csv2tab(line);
      line = switchNA(line,"NA","-9999");
      istringstream lin(line.c_str());
      lin>>d.year;
      lin>>d.decday;
      d.decday = d.decday-tlag;
      if(d.decday < 0.0){ d.year--; d.decday += firstday(13,d.year);}
      if(d.decday >firstday(13,d.year))  {d.decday -= firstday(13,d.year); d.year++;} 
      if(firstpass){year_first = d.year; day_first = d.decday;firstpass=0;}
      lin>>w;
      lin>>ustar;  //u*
      lin>>wdir;
      lin>>par;
      lin>>T;
      if(T > fNA){
	T = T + Kelvin;
	svp = SatVapPres(T);
      } else {
	notemp=1;
      }
      lin>>x; //daily min temp
      lin>>d.soilT; //soil temp
      lin>>vpd;
      lin>>d.cflux;
      lin>>x; //estimated C flux
      if(d.cflux <= fNA || wdir <180 || wdir > 270 || ustar < 0.4){
	d.cflux = x;  //use estimated flux rather than measured
      } 

      //calculate required values
      if(w > fNA && wdir > fNA){
	d.u = w*cos(wdir*radians);
	d.v = w*sin(wdir*radians);
      } else { d.u = d.v = fNA;}
      d.theta = T;
      if(par > fNA){
	d.rshort = par*C_PARCONV;
      } else {d.rshort = fNA;}
      if(!notemp && vpd > fNA){
	d.rv = rh2rv((1-vpd/svp),T);
      } else {
	d.rv = fNA;
      }
      //      cout<<d.rv<<"\t"<<(100*(1-vpd/svp))<<"\t"<<vpd<<"\t"<<svp<<endl;
      data.push_back(d);
      year_last = d.year;
      day_last  = d.decday;
    }
    in.close();

    // *************  LOAD SECOND FILE (2004) ***************** //
    if(checkfileR(file2.c_str())){
      in2.open(file2.c_str());
    } else {cerr<<file2<<" not found"<<endl; abort();}
    //remove header
    getline(in2,line);
    getline(in2,line);
    getline(in2,line);
    //load data
    while (getline(in2,line)){
      notemp=0;

      LSMtimepoint d;
      //clean line
      csv2tab(line);
      line = switchNA(line,"NA","-9999");
      istringstream lin(line.c_str());
      d.year=2004;
      lin>>d.decday;
      d.decday = d.decday-tlag;
      if(d.decday < 0.0){ d.year--; d.decday += firstday(13,d.year);}
      if(d.decday >firstday(13,d.year))  {d.decday -= firstday(13,d.year); d.year++;} 
      lin>>d.cco2; //CO2 (mmol/mol??)
      lin>>d.ch20; //H20 (mmol/mol)
      lin>>w;
      lin>>ustar;  //u*
      lin>>wdir;
      lin>>d.Hflux; // H  (w/m2)
      lin>>x; // LE (w/m2)
      lin>>x; //raw C flux
      lin>>d.cflux;  //C flux filtered by direction and ustar
      //if(d.cflux <= fNA || wdir <180 || wdir > 270 || ustar < 0.4){
      lin>>x; // NEE (mmol/m2/s)
      lin>>x; // NEE indicator (1=data,0=estimate)
      lin>>x; // R estimate (mmol/m2/s)
      lin>>x; // R estimate (min 0) (mmol/m2/s)
      lin>>x; // GEE estimate (mmol/m2/s)
      lin>>x; // raw FH20 (mmol/m2/s)
      lin>>d.Wflux; // filtered FH20 (mmol/m2/s)
      lin>>x; // Sonic Tair (C)
      lin>>T; //Tair.above.canopy (C)
      lin>>rh; //RH.above.canopy (%)
      lin>>x; //Tair.above.canopy (TC) (C)
      lin>>vpd; //VPD above canopy (kPa, min 0)
      lin>>d.soilT; //soil temp (C, @10cm)
      lin>>par;  //mmol/m

      //calculate required values
      if(T > fNA){
	T = T + Kelvin;
	svp = SatVapPres(T);
	if(T > 320){
	  cout<<"ERROR IN Temperature :: "<<d.year<<"\t"<<d.decday<<"\t"<<T<<endl;
	  exit(2);
	}
      } else {
	notemp=1;
      }
      if(w > fNA && wdir > fNA){
	d.u = w*cos(wdir*radians);
	d.v = w*sin(wdir*radians);
      } else { d.u = d.v = fNA;}
      d.theta = T;
      if(par > fNA){
	d.rshort = par*C_PARCONV;
      } else {d.rshort = fNA;}
      if(!notemp){
	if(rh > fNA){
	  d.rv = rh2rv(rh/100.0,T);
	} else if(vpd > fNA){
	  d.rv = rh2rv(1-vpd/svp,T);
	} else {
	  d.rv = fNA;
	}
      } else {
	d.rv = fNA;
      }
    }
    //load 2005-2006 data (MET ONLY)
    ifstream in3;
    if(checkfileR(file3.c_str())){
      in3.open(file3.c_str());
      cout<<"load "<<file3<<endl;
    } else {cerr<<file3<<" not found"<<endl; abort();}
    //remove header
    getline(in3,line);
    //load data
    while (getline(in3,line)){
      notemp=0;
      
      LSMtimepoint d;
      //clean line
      csv2tab(line);
      line = switchNA(line,"NA","-9999");
      istringstream lin(line.c_str());
      lin>>d.year;
      lin>>d.decday;
      d.decday = d.decday-tlag;
      if(d.decday < 0.0){ d.year--; d.decday += firstday(13,d.year);}
      if(d.decday >firstday(13,d.year))  {d.decday -= firstday(13,d.year); d.year++;} 
      lin>>par;  //mmol/m	
      lin>>T; // Tair (C)
      lin>>rh; //RH.above.canopy (%)
      lin>>vpd; //VPD above canopy (kPa, min 0)
      lin>>x; // Tair TC (C)
      //calculate required values
      if(T > fNA){
	T = T + Kelvin;
	svp = SatVapPres(T);
	if(T > 320){
	  cout<<"ERROR IN Temperature :: "<<d.year<<"\t"<<d.decday<<"\t"<<T<<endl;
	  cout<<line<<endl;
	  exit(2);
	}
	notemp=0;
      } else {
	notemp=1;
      }
      d.u = d.v = fNA;	
      d.theta = T;
      if(par > fNA){
	d.rshort = par*C_PARCONV;
      } else {d.rshort = fNA;}
      if(!notemp){
	if(rh > fNA){
	  d.rv = rh2rv(rh/100.0,T);
	} else if(vpd > fNA){
	  d.rv = rh2rv(1-vpd/svp,T);
	} else {
	  d.rv = fNA;
	}
      } else {
	d.rv = fNA;
      }
      data.push_back(d);
      year_last = d.year;
      day_last  = d.decday;
    }
    in3.close();
    
  } /// END harvard_tsca

  if(folder == "duke_hw"){  // DUKE HARDWOOD TOWER (data from stoy, 7/06)
    foundFolder = 1;

    //set coordinates
    lat = 35.97358;
    lon = -79.10043; 

    //set default timestep (fractional days)
    tstep = 1.0/24;

    //set time offset (difference from UTC)
    tlag = -5.0/24.0;

    //mark which data loaded
    ismissing[0] = ismissing[1] = ismissing[3] = ismissing[4] = ismissing[5] = ismissing[6] = ismissing[10] = ismissing[12] =ismissing[13] = 0;
    //set files
    string line;
    file = folder+"/Ameriflux_h_0606.txt";
    if(checkfileR(file.c_str())){
      in.open(file.c_str());
    } else {cerr<<file<<" not found"<<endl; abort();}

    //load data
    float w,wdir,vpd,T,par,x,svp,rshort,ppt,day,hr,year;
    bool notemp(0),firstpass(1);
    while (getline(in,line)){
      notemp=0;
      LSMtimepoint d;
      //clean line
      line = switchNA(line,"0.0000000e+000","-9999");
      istringstream lin(line.c_str());
      lin>>year;  d.year = (int)year;
      lin>>day;
      lin>>hr;    d.decday = day+hr/24-tlag-1;
      //      if(firstpass) {cout<<"First "<<d.decday<<" "<<d.year<<" "<<year<<" "<<day<<" "<hr<<" "<<tlag<<endl;}
      if(firstpass) {cout<<year<<" "<<d.year<<" "<<d.decday<<endl;}
      if(d.decday < 0.0){ d.year--; d.decday += firstday(13,d.year);}
      if(firstpass) {cout<<year<<" "<<d.year<<" "<<d.decday<<" "<<firstday(13,d.year)<<endl;}
      if(d.decday >firstday(13,d.year))  {d.decday -= firstday(13,d.year); d.year++;} 
      if(firstpass){year_first = d.year; day_first = d.decday;firstpass=0; cout<<"First "<<d.decday<<" "<<d.year<<" "<<year<<" "<<day+hr/24-tlag-1<<endl;}
      lin>>x;  // DOY2
      lin>>x;  // month
      lin>>x;  // month2
      lin>>x;  // zenith
      lin>>d.cflux;  // Fc (mgC/m2/s)
      //      d.cflux *= 315576; //kgC/ha/yr
      lin>>d.gpp;  //GEP, est canopy photosynth, mgC/m2/s
      //d.gpp*=315576;
      lin>>x;  // RE, ecosystem respiration
      lin>>x;  // Fcgfp, C flux indicator function (1 for raw data, 0 for gapfilled)
      lin>>d.Hflux;  //sensible heat flux (W m-2)
      lin>>x;  // Hgpf    %sonic anemometer indicator function (use also with ustar and WS)
      lin>>x;  // LE      %Latent heat flux (W m-2)
      lin>>x;  // LEgpf   %Latent heat gapfilling indicator function
      lin>>x;  // Evapotranspiration (mm)
      lin>>d.transp;  // Estimated Transpiration (mm)
      lin>>d.evap;  // Estimated (Soil) Evaporation (mm)
      lin>>x;  // Canopy conductance (mol H2O m-2 s-1)
      lin>>x;  //u*
      lin>>w;  //wind speed (m s-1)
      lin>>wdir; //wind direction (degrees)
      lin>>d.lai;  //lai
      lin>>ppt;  //precipitation (mm)
      if(ppt < 0.0) ppt = 0.0; 
      d.pcpg = ppt/1800; // precip mm/30min -> kg/m2/s
      lin>>par; //Photosynthetically active radiation micromole/m2/s
      lin>>x;  //Rn Net radiation. Q7 sensor before '04, CNR1 thereafter. W m-2
      lin>>rshort;  //SWup
      lin>>x;  //SWdown
      lin>>d.rlong; //LWup
      lin>>x;  //LWdown
      lin>>x;  //xp peak of source-weight function (m)
      lin>>x;  //STAB
      lin>>x;  //FETCH
      lin>>d.Tcanopy;  //Ta Air Temperature at 2/3 canopy height (C)
      lin>>x;  //VPD
      lin>>T;  //Ttop
      if(T > fNA){
	T = T + Kelvin;
	svp = SatVapPres(T);
      } else {
	notemp=1;
      }
      lin>>vpd;
      lin>>x; //yt

      //calculate required values
      if(w > fNA && wdir > fNA){
	d.u = w*cos(wdir*radians);
	d.v = w*sin(wdir*radians);
      } else { d.u = d.v = fNA;}
      d.theta = T;
      if(rshort >= 0.0){
	d.rshort = rshort;
      } else {
	if(par > fNA){
	  d.rshort = par*0.5343648; //site specific correction
	} else {d.rshort = fNA;}
      }
      if(!notemp && vpd > fNA){
	d.rv = rh2rv((1-vpd/svp),T);
      } else {
	d.rv = fNA;
      }
      d.rdiff = fNA;
      data.push_back(d);
      year_last = d.year;
      day_last  = d.decday;
    }
  } /// END duke_hw

  if(amerifluxParser){  //Generic processor for level 3 & 4 flux data
    foundFolder=1;
    folder = file;
    elev = fNA;
    tstep= 1.0/24.0;  // set to slightly more than an hour to make sure we interpolate hourly data

    //mark which met data is loaded
    ismissing[0] = ismissing[1] = ismissing[3] = ismissing[4] = ismissing[5] = ismissing[6]  = ismissing[9] = ismissing[10] = ismissing[11] = ismissing[13] = 0;

    //hack to turn off ppt
    //    ismissing[9] = ismissing[10] = 1;

    //load data
    float x,Rpot;
    int yr;
    char cyr[4];
    bool notemp(0),firstpass(1);
    for(yr=yr_start;yr <= yr_stop;yr++){

      cout<<"Loading Year "<<yr<<endl;

      string fullfile;
      fullfile = file + "/" + prefix;
      itoa(yr,cyr);
      fullfile += cyr;
      fullfile += "_L";
      if(amerifluxLevel == 4) {
	fullfile += "4_h.txt";
      }else{
	fullfile += "3.txt";
      }

      ifstream inAM;
      if(checkfileR(fullfile.c_str())){
	inAM.open(fullfile.c_str());
      } else {cerr<<fullfile<<" not found"<<endl; abort();}
     
      cout<<fullfile<<" open"<<endl;

      //remove header
      string line;
      getline(inAM,line);
    
      //load data
      while(getline(inAM,line)){
	notemp=0;
	
	LSMtimepoint d;
	csv2tab(line);
	line = switchNA(line,"NA","-9999.0");
	istringstream lin(line.c_str());
	d.year = yr;  //set year based on file
	lin>>x;   //Month
	lin>>x;	  //Day
	lin>>x;   //Hour	
	lin>>d.decday; //Decimal Day
	//extract date
	d.decday -= tlag - 1.0;  //adjust day by 1.0 to make january 1 start at 0
	if(d.decday < 0.0){ d.year--; d.decday += firstday(13,d.year);}
	if(d.decday >firstday(13,d.year))  {d.decday -= firstday(13,d.year); d.year++;} 
	if(firstpass){year_first = d.year; day_first = d.decday;firstpass=0;}
	lin>>d.sclp;  //CO2 (umol/mol)
	lin>>x;	      //H20
	lin>>x;       //Zl
	lin>>d.cflux; //Fc = CO2 flux (umol/m2/s)
	lin>>x;       //gf_Fc
	lin>>x;       //NEE_st
	lin>>x;       //qf_NEE_st
	lin>>x;       //NEE_or
	lin>>x;       //qf_NEE_or
	lin>>d.Hflux; //H  = Sensible heat flux (W/m2)
	lin>>x;       //LE = Latent heat flux (W/m2)
	lin>>x;       //ustar
	lin>>x;       //qf_ust
	lin>>d.pcpg;  //x;
	//d.pcpg = fNA;   //precip (mm)  -> time units
	if(d.pcpg > fNA){
	  d.conprr = 0.0;
	  d.pcpg = d.pcpg/1800;   //convert mm/30min -> kg/m2/s
	}else {
	  d.conprr = fNA;
	}
	lin>>d.rshort; //Rg (W/m2)
	lin>>par;     //PAR_in = umol/ms/s
	if(d.rshort < 0){  //if Rg is unavailable, set Rshort based on PAR
	  if(par > fNA){
	    d.rshort = par*C_PARCONV;
	  } else {
	    d.rshort = fNA;
	  }
	} else {d.rshort = fNA;}
	lin>>Rpot;
	lin>>x;       //qf_Rg
	lin>>x;       //qf_Rad
	lin>>x;       //Rr
	lin>>x;       //Rn
	lin>>d.rdiff; //diffuse 
	if(d.rdiff < 0.0){  //if diffuse unavailable, set based on pot'n
	  if(d.rshort > 0 && Rpot > 0){
	    d.rdiff = d.rshort*(1-max(0.0,min(0.9,d.rshort/Rpot)));
	  } else {
	    d.rdiff = fNA;
	  }
	}
	lin>>x;      //APAR
	lin>>T;       //TA = air temperature (C)
	if(T > fNA){
	  T = T + Kelvin;
	}
	lin>>d.soilT;    // Soil temperature (C) 
	lin>>x;      //Tsoil2
	lin>>d.soilW;    // Soil moisture (%)       ####### DEPTH??  
	lin>>x;      //SWC2
	lin>>d.sensgc; //G1 = Soil heat flux (W/m2)
	lin>>x;      //G2
	lin>>rh;      //relative humidity
	if(rh>fNA&&T>fNA){
	  d.rv = rh2rv(rh/100.0,T);
	  if(d.rv > 1){
	    cout<<"ERROR IN HUMIDITY\t"<<rh<<"\t"<<d.rv<<"\t"<<T<<endl;
	    cout<<line<<endl;
	    exit(2);
	  }
	} else {
	  d.rv = fNA;
	}

	lin>>wdir;       // Wind direction
	lin>>w;          // Wind speed (scalar mean)
	if(w > fNA && wdir > fNA){
	  d.u = w*cos(wdir*radians);
	  d.v = w*sin(wdir*radians);
	} else { d.u = d.v = fNA;}

	d.theta = T;

	data.push_back(d);	
	year_last=d.year;
	day_last=d.decday;
      }
      inAM.close();
    } //end loop over file year

    cout<<"loaded"<<endl;

  }  // end Ameriflux Parser


  ///***************************************************************///
  ///*                                                             *///
  ///*               START FILTERING                               *///
  ///*                                                             *///
  ///***************************************************************///


  if(!foundFolder){
    cout<<"DATA SET "<<folder<<" NOT RECOGNIZED"<<endl;
  }

  int month_first(day2month(day_first,year_first));
  int month_last(day2month(day_last,year_last));
  float solar_tlag(lon/360.0*86400); //time diff from GMT in seconds
  float fstep;

  //  cout<<"lag"<<tlag<<endl;
  //  cout<<solar_tlag<<endl;
  //  exit(2);

  // write out back to folder

  //  year_first--;

  cout<<"LOADED FLUX TOWER DATA "<<data.size()<<endl;
  cout<<day_first<<" "<<month_first<<" "<<year_first<<" to "<<day_last<<" "<<month_last<<" "<<year_last<<endl;

  //**** DAT ********
  int count(0); //data stream counter
  double stop_time; //time to read each bin to
  double ncep_time; //time in ncep file
  if(do_dat){
  // loop over years
  for(int year = year_first;year <= year_last; year++){
    stop_time = dt; 
    //set year name
    ostringstream ystrm (ostringstream::out); 
    ystrm << year;
    string yname = ystrm.str(); yname = yname.substr(0,4);
    //set month range
    int mstart(1),mstop(12);
    if(year==year_first) { 
      mstart=month_first;
      float day(data[0].decday);
      //stop_time = ceil((day-floor(day))/dt)*dt+floor(day);
      stop_time = firstday(mstart,year)*86400+dt;
    }
    if(year==year_last)  mstop = month_last;
    cout<<year<<": months "<<mstart<<" "<<mstop<<endl;

    //loop over months
    for(int mo = mstart;mo <= mstop; mo++){

      //      int firstmonth = 1;

      //open output file
      ostringstream mstrm (ostringstream::out); mstrm << mo;
      string mname = mstrm.str();
      if(mname.length() == 1) mname = "0" + mname;
      string datname = folder + "/" + mname + yname + ".dat";
      cout<<datname<<endl;
      ofstream dat(datname.c_str());

      //write dat header  -> don't write for SOI
      if(metheader){
	dat<<mo<<"\t"<<year<<"\t1\t1"<<endl;
	dat<<"1\t1\t"<<floor(lat)<<"\t"<<floor(lon)<<endl;
      }

      //open NCEP file & parse header
      string ncep_filename = ncepPath + mname + yname + ".dat";
      cout<<ncep_filename<<endl;
      ifstream ncep(ncep_filename.c_str());
      string ncepLine;
      int nlat,nlon,fmo;
      float rlat,rlon,ll_lat,ll_lon,x;
      if(!readstraight){
	//get info from header
	getline(ncep,ncepLine);
	cout<<"ncephead1\t"<<ncepLine<<endl;
	istringstream ncepHead1(ncepLine);
	ncepHead1>>fmo;
	ncepHead1>>x;
	ncepHead1>>nlat;
	ncepHead1>>nlon;
	getline(ncep,ncepLine);
	cout<<"ncephead2\t"<<ncepLine<<endl;
	istringstream ncepHead2(ncepLine);
	ncepHead2>>rlat;
	ncepHead2>>rlon;
	ncepHead2>>ll_lat;
	ncepHead2>>ll_lon;
	//skip to first row needed
	int nrow = static_cast<int>(nlat-floor((lat-ll_lat)/rlat));
	int ncol = static_cast<int>(floor((lon-ll_lon)/rlon));

	//	cout<<(lon-ll_lon)/rlon<<endl;
	//cout<<(lat-ll_lat)/rlat<<endl;

	int nbegin = 4*nday(fmo,year)*(ncol+nlon*nrow); //number of lines to the first line that's the correct location
	//nbegin = 16*nday(fmo,year);
	for(i=0;i<nbegin;i++) getline(ncep,ncepLine);
	cout<<ncepLine<<endl;
	cout<<nlon<<"\t"<<nlat<<"\t"<<rlon<<"\t"<<rlat<<"\t"<<ll_lon<<"\t"<<ll_lat<<endl;
	cout<<"\tnbegin="<<nbegin<<endl;
	cout<<"nrow = "<<nrow<<"\tncol= "<<ncol<<"\tnday = "<<nday(fmo,year)<<"\t"<<fmo<<"\t"<<mo<<endl;
      }	
      int nskip = 0; //number of lines between lines of interest
      //      if(year == year_first && mo == mstart){
      // i = 0;
// 	while(i < (int)nskip*floor((data[0].decday-firstday(mo,year))*4)){
// 	  getline(ncep,ncepLine);
// 	  dat<<ncepLine<<endl; 	//output entries before start of observed data
// 	  for(int j=0;j<nskip;j++,i++){getline(ncep,ncepLine);}
// 	}
//       }
      ncep_time = firstday(mo,year)*86400;


      //loop over data for month
      LSMtimepoint dncep;
      int ncepcounter = 0;
      while(count<static_cast<int>(data.size())){

	if(stop_time > firstday(mo+1,year)*86400) break; //end processing month

	//start new bin
	LSMtimepoint dout;
	for(i=0;i<14;i++) n_data[i]=0;

	//	if(firstmonth) cout<<"IC\t"<<dout<<endl;

	//	cout<<"ncep\t"<<ncep_time<<endl;
	while(ncep_time < stop_time){
	  //read ncep, advance counter
	  getline(ncep,ncepLine);
	  cout<<ncepLine<<endl;
	  istringstream nin(ncepLine);
	  nin>>dncep.geoht;
	  nin>>dncep.u;
	  nin>>dncep.v;
	  nin>>dncep.theta;
	  nin>>dncep.rv;
	  nin>>dncep.pi0;
	  nin>>dncep.dn0;
	  nin>>dncep.conprr;
	  nin>>dncep.pcpg;
	  nin>>dncep.sclp;
	  nin>>dncep.rlong;
	  nin>>dncep.rshort;
	  //	  for(i=0;i<nskip;i++) getline(ncep,ncepLine);
	  ncep_time += dtncep;
	  ncepcounter++;
	}
	//	cout<<ncep_time<<"\t"<<stop_time<<endl;

	//	cout<<data[count].decday<<"\t"<<data[count].year<<endl;

	while(data[count].decday*86400 <= stop_time && data[count].year == year){

	  //summation
	  if(data[count].geoht > fNA){
	    dout.geoht += data[count].geoht;
	    n_data[2]++;
	  }
	  if(data[count].u > fNA){
	    dout.u += data[count].u;
	    n_data[3]++;
	  }
	  if(data[count].v > fNA){
	    dout.v += data[count].v;
	    n_data[4]++;
	  }
	  if(data[count].theta > fNA){
	    dout.theta += data[count].theta;
	    n_data[5]++;
	  }
	  if(data[count].rv > fNA){
	    dout.rv += data[count].rv;
	    n_data[6]++;
	  }
	  if(data[count].pi0 > fNA){
	    dout.pi0 += data[count].pi0;
	    n_data[7]++;
	  }
	  if(data[count].dn0 > fNA){
	    dout.dn0 += data[count].dn0;
	    n_data[8]++;
	  }
	  if(data[count].conprr > fNA){
	    dout.conprr += data[count].conprr;
	    n_data[9]++;
	  }
	  if(count%progress_freq == 0){
	    cout<<count<<" of "<<data.size()<<"\t"<<year<<"\t"<<mo<<"\t"<<data[count].pcpg<<endl;
	  }
	  if(data[count].pcpg > fNA){
	    dout.pcpg += data[count].pcpg;
	    n_data[10]++;
	  }
	  if(data[count].sclp > fNA){
	    dout.sclp += data[count].sclp;
	    n_data[11]++;
	  }	  
	  if(data[count].rlong > fNA){
	    dout.rlong += data[count].rlong;
	    n_data[12]++;
	  }
	  if(data[count].rshort > fNA){
	    dout.rshort += data[count].rshort;
	    n_data[13]++;
	  }
	  count++;
	}
	
	//INTERPOLATION
	//	cout<<count<<"\t"<<(data[count].decday-data[count-1].decday)<<"\t"<<tstep<<"\t"<<data[count].year<<"\t"<<data[count-1].year<<endl;
	//	cin>>fstep;

	if(count > 0 && (data[count].decday-data[count-1].decday) <= tstep && data[count].year==data[count-1].year){
	  fstep = (stop_time/86400 - data[count-1].decday)/(data[count].decday-data[count-1].decday);
	  //cout<<"INTERPOLATE\t"<<fstep<<endl;
	  if(n_data[2] == 0 && data[count].geoht > fNA && data[count-1].geoht > fNA){
	    dout.geoht = (fstep*data[count].geoht+(1-fstep)*data[count-1].geoht);
	    n_data[2]++; 
	  }
	  if(n_data[3] == 0 && data[count].u > fNA && data[count-1].u > fNA){
	    dout.u = (fstep*data[count].u+(1-fstep)*data[count-1].u);
	    n_data[3]++; 
	  }
	  if(n_data[4] == 0 && data[count].v > fNA && data[count-1].v > fNA){
	    dout.v = (fstep*data[count].v+(1-fstep)*data[count-1].v);
	    n_data[4]++; 
	  }
	  if(n_data[5] == 0 && data[count].theta > fNA && data[count-1].theta > fNA){
	    dout.theta = (fstep*data[count].theta+(1-fstep)*data[count-1].theta);
	    n_data[5]++; 
	  }
	  if(n_data[6] == 0 && data[count].rv > fNA && data[count-1].rv > fNA){
	    dout.rv = (fstep*data[count].rv+(1-fstep)*data[count-1].rv);
	    n_data[6]++; 
	  }
	  if(n_data[7] == 0 && data[count].pi0 > fNA && data[count-1].pi0 > fNA){
	    dout.pi0 = (fstep*data[count].pi0+(1-fstep)*data[count-1].pi0);
	    n_data[7]++; 
	  }
	  if(n_data[8] == 0 && data[count].dn0 > fNA && data[count-1].dn0 > fNA){
	    dout.dn0 = (fstep*data[count].dn0+(1-fstep)*data[count-1].dn0);
	    n_data[8]++; 
	  }
	  if(n_data[9] == 0 && data[count].conprr > fNA && data[count-1].conprr > fNA){
	    dout.conprr = (fstep*data[count].conprr+(1-fstep)*data[count-1].conprr);
	    n_data[9]++; 
	  }
	  if(n_data[10] == 0 && data[count].pcpg > fNA && data[count-1].pcpg > fNA){
	    dout.pcpg = (fstep*data[count].pcpg+(1-fstep)*data[count-1].pcpg);
	    n_data[10]++; 
	  }
	  if(n_data[11] == 0 && data[count].sclp > fNA && data[count-1].sclp > fNA){
	    dout.sclp = (fstep*data[count].sclp+(1-fstep)*data[count-1].sclp);
	    n_data[11]++; 
	  }
	  if(n_data[12] == 0 && data[count].rlong > fNA && data[count-1].rlong > fNA){
	    dout.rlong = (fstep*data[count].rlong+(1-fstep)*data[count-1].rlong);
	    n_data[12]++; 
	  }
	  if(n_data[13] == 0 && data[count].rshort > fNA && data[count-1].rshort > fNA){
	    //cout<<"INTERP"<<data[count].rshort<<"\t"<<data[count-1].rshort<<"\t"<<fstep<<endl;
	    dout.rshort = (fstep*data[count].rshort+(1-fstep)*data[count-1].rshort);
	    n_data[13]++; 
	    //exit(1);
	  }
	}
	  
	//calculate means
	dout.geoht  = dout.geoht/n_data[2];
	dout.u      = dout.u/n_data[3];
	dout.v      = dout.v/n_data[4];
	dout.theta  = dout.theta/n_data[5];
	dout.rv     = dout.rv/n_data[6];
	dout.pi0    = dout.pi0/n_data[7];
	dout.dn0    = dout.dn0/n_data[8];
	dout.conprr = dout.conprr/n_data[9];
	dout.pcpg   = dout.pcpg/n_data[10];
	dout.sclp   = dout.sclp/n_data[11];
	dout.rlong  = dout.rlong/n_data[12];
	dout.rshort = dout.rshort/n_data[13];

	//fill missing
	for(i=0;i<14;i++){
	  // cout<<i<<"\t"<<ismissing[i]<<"\t"<<n_data[i]<<endl;
	  if(ismissing[i]||n_data[i]==0){
	    switch(i){
	    case 2:
	      dout.geoht = dncep.geoht;
	      break;
	    case 3:
	      dout.u = dncep.u;
	      break;
	    case 4:
	      dout.v = dncep.v;
	      break;
	    case 5:
	      dout.theta = dncep.theta;
	      break;
	    case 6:
	      dout.rv = dncep.rv;
	      break;
	    case 7:
	      dout.pi0 = dncep.pi0;
	      break;
	    case 8:
	      dout.dn0 = dncep.dn0;
	      break;
	    case 9:
	      dout.conprr = dncep.conprr;
	      break;
	    case 10:
	      dout.pcpg = dncep.pcpg;
	      break;
	    case 11:
	      if(dncep.sclp > 0){
		dout.sclp = dncep.sclp;
	      } else {
		dout.sclp = 370;   //set default
	      }
	      break;
	    case 12:
	      dout.rlong = dncep.rlong;
	      break;
	    case 13:
	      dout.rshort = dncep.rshort;
	      break;
	    }
	  }
	}

	//write out periodic value
	dat<<dout<<endl;
	//	cout<<dout<<endl;
	//	cout<<dncep<<endl;
	//if(firstmonth) cout<<"end\t"<<dout<<endl;
	//	dat<<stop_time<<"\t"<<dout<<endl;

	//set new stop time
	stop_time += dt;
	//firstmonth = 0;	

      }  //end load timest
      //      exit(1);

      if(year == year_last && mo == month_last){ //finish month off w/ NCEP data
	while(getline(ncep,ncepLine)){
	  if(ncepcounter >= nday(mo,year)*86400/dtncep) break;
	  dat<<ncepLine<<endl;
	  ncepcounter++;
	  for(i=0;i<nskip;i++) getline(ncep,ncepLine);	  
	}
      }
      ncep.close();
      dat.close();
      cout<<"**ncount\t"<<ncepcounter<<endl;

    } //end month loop
    //make sure to advance to next year
    while(count<static_cast<int>(data.size())&&data[count].year == year){
      cout<<"Advance past "<<data[count]<<endl;
      count++;
    }

  }   //end year loop
  }


  //***********  RAD.DAT *******************//
  if(do_rad){
  cout<<"PROCESSING RADIATION FILES"<<endl;
  
  //loop over years again
  bool sens=0;
  double dfrac;
  int solar_day;
  float solar_sec;
  count=0; //data stream counter
  float timeinc = 86400/dtrad;  //15min default
  for(int year = year_first;year <= year_last; year++){
    stop_time = timeinc; 
    ostringstream ystrm (ostringstream::out); 
    ystrm << year;
    string yname = ystrm.str(); yname = yname.substr(0,4);
    //set month range
    int mstart(1),mstop(12);
    if(year==year_first) { 
      mstart=month_first;
      //float day(data[0].decday);
      float day = day_first;
      stop_time = firstday(mstart,year)*86400+timeinc;
      //ceil((day-floor(day))/timeinc)*timeinc+floor(day);
    }
    if(year==year_last)  mstop =month_last;
    cout<<year<<": months "<<mstart<<" "<<mstop<<endl;

    for(int mo = mstart;mo <= mstop; mo++){

      //      if(mo==3 && year==2001) {sens=1;} else {sens=0;}

      //open output file
      ostringstream mstrm (ostringstream::out); mstrm << mo;
      string mname = mstrm.str();
      if(mname.length() == 1) mname = "0" + mname;
      string datname = folder + "/" + mname + yname + "-rad.dat";
      cout<<datname<<endl;
      ofstream dat(datname.c_str());

      //write dat header
      if(radheader){
	dat<<mo<<"\t"<<year<<"\t1\t1"<<endl;
	dat<<"1\t1\t"<<floor(lat)<<"\t"<<floor(lon)<<endl;
      }

      //open NCEP derived RAD file & parse header
      string ncep_filename = ncepPath + mname + yname + "-rad.dat";
      cout<<ncep_filename<<endl;
      ifstream ncep(ncep_filename.c_str());
      string ncepLine;
      int nlat,nlon,fmo;
      float rlat,rlon,ll_lat,ll_lon,x;
      getline(ncep,ncepLine);
      cout<<ncepLine<<endl;
      istringstream ncepHead1(ncepLine);
      ncepHead1>>x;
      ncepHead1>>fmo;
      ncepHead1>>nlat;
      ncepHead1>>nlon;
      getline(ncep,ncepLine);
      cout<<ncepLine<<endl;
      istringstream ncepHead2(ncepLine);
      ncepHead2>>rlat;
      ncepHead2>>rlon;
      ncepHead2>>ll_lat;
      ncepHead2>>ll_lon;

      //      int nskip = nlon*nlat - 1; //number of lines between lines of interest
      int nskip = 0; //number of lines between lines of interest
      int nrow = static_cast<int>(nlat-floor((lat-ll_lat)/rlat));
      int ncol = static_cast<int>(floor((lon-ll_lon)/rlon));	
      int nbegin = 96*nday(fmo,year)*(ncol+nlon*nrow); //number of lines to the first line that's the correct location
      if(rlat == 1 && rlon == 1) {nbegin=0;}
      //      int nbegin = static_cast<int>(floor((lon-ll_lon)/rlon)+nlon*(nlat-floor((lat-ll_lat)/rlat)-1)); //number of lines to the first line that's the correct location
      for(i=0;i<nbegin;i++) getline(ncep,ncepLine);
      if(year == year_first && mo == mstart){
	i = 0;
	while(i < (int)nskip*floor((data[0].decday-firstday(mo,year))*48)){
	  getline(ncep,ncepLine);
	  dat<<ncepLine<<endl; 	//output entries before start of observed data
	  //for(int j=0;j<nskip;j++,i++){getline(ncep,ncepLine);}
	  i++;
	}
      }

      cout<<nlon<<"\t"<<nlat<<"\t"<<rlon<<"\t"<<rlat<<"\t"<<ll_lon<<"\t"<<ll_lat<<endl;
      cout<<"nskip = "<<nskip<<"\tnbegin="<<nbegin<<"\t"<<endl;

      while(count<static_cast<int>(data.size())){
	//cout<<count<<"\t"<<stop_time<<"\t"<<firstday(mo+1,year)<<endl;
	if(stop_time > firstday(mo+1,year)*86400) break; //end processing month

	//start new bin
	int src(0);
	float dout[2],dncep[2];
	n_data[0]=n_data[1]=0;
	dout[0]=dout[1] = 0.0;

	//read ncep, advance counter
	getline(ncep,ncepLine);
	istringstream nin(ncepLine);
	nin>>dncep[0];
	nin>>dncep[1];
	//for(i=0;i<nskip;i++) getline(ncep,ncepLine);
	
	//	if(sens) cout<<"A\t"<<count<<"\t"<<data[count].decday<<"\t"<<data[count].year<<"\t"<<data[count-1].decday<<"\t"<<data[count-1].year<<"\t"<<stop_time<<endl;

	//advance to correct year if nessisary
	while(data[count].year < year &&count<static_cast<int>(data.size())){
	  count++;
	}

	//15 min loop - summation/interpolation
	while(data[count].decday*86400 < stop_time && data[count].year == year){
	  //data available and has to be averaged
	  if(data[count].rshort > fNA){
	    dout[0] += data[count].rshort;
	    n_data[0]++;
	    src=1;
	  }
	  if(data[count].rdiff > fNA){
	    dout[1] += data[count].rdiff;
	    n_data[1]++;
	  }
	  count++;
	}
	
	if(sens) cout<<"B\t"<<count<<"\t"<<dout[0]<<"\t"<<dout[1]<<"\t"<<n_data[0]<<"\t"<<n_data[1]<<endl;


	//INTERPOLATION
	if(n_data[0] == 0 && count > 0 && (data[count].decday-data[count-1].decday) < tstep && data[count].year==data[count-1].year){
	  if(data[count].rshort > fNA && data[count-1].rshort > fNA){
	    dout[0] = (data[count].rshort+data[count-1].rshort)/2;
	    n_data[0]++; 
	    src=2;
	  }
	  if(n_data[1] == 0 && data[count].rdiff > fNA && data[count-1].rdiff > fNA){
	    dout[1] = (data[count].rdiff+data[count-1].rdiff)/2;
	    n_data[1]++; 
	  }
	}

	if(sens) cout<<"C\t"<<count<<"\t"<<dout[0]<<"\t"<<dout[1]<<"\t"<<n_data[0]<<"\t"<<n_data[1]<<endl;

	//averaging
	dout[0] = dout[0]/n_data[0];
	dout[1] = dout[1]/n_data[1];

	//fill missing
	if(n_data[0]==0){
	  //data not available, has to be filled
	  dout[0] = dncep[0];
	  dout[1] = dncep[1];  //automatically disregard diffuse if direct is absent
	  src=4;
	} else { //direct data available
	  if(n_data[1]==0){
	    //diffuse not available, estimate from direct
	    solar_day = (int)floor((stop_time+solar_tlag)/86400.0);
	    solar_sec = stop_time-0.5*timeinc+solar_tlag-solar_day*86400.0;
	    dfrac = dirfrac(solar_day,solar_sec,dout[0],lat);
	    dout[1] = dout[0]*(1.0-dfrac); 
	    //	    cout<<dout[0]<<"\t"<<dout[1]<<"\t"<<dfrac<<"\t"<<stop_time<<"\t"<<solar_day<<"\t"<<solar_sec<<endl;
	    // if(dfrac>0.01&&dout[0]>20){
	       //   dout[1]=dout[0]*((1/dfrac)-1);
	  }
	    //dout[1] = dncep[1];
	    //}
	    //cout<<dfrac<<"\t"<<dout[0]<<"\t"<<dout[1]<<endl;
	    //dout[1] = (1-dfrac)*dout[0];
	    //dout[0] *= dfrac;
	 
	}

	//error check
	if(dout[0]> 1367.0){dout[0] = 1367.0;}
	if(dout[0] < 0.0) dout[0] = 0.0;
	if(dout[1] < 0.0) dout[1] = 0.0;
	if(dout[1] > dout[0]){//swap
	  dfrac = dout[1];
	  dout[1] = dout[0];
	  dout[0] = dfrac;
	}

	if(sens) cout<<"D\t"<<count<<"\t"<<dout[0]<<"\t"<<dout[1]<<"\t"<<n_data[0]<<"\t"<<n_data[1]<<endl;

	//write out value
	dat<<dout[0]<<"\t"<<dout[1]<<"\t"<<src<<endl;
	if(sens) cout<<dout[0]<<"\t"<<dout[1]<<"\t"<<dncep[0]<<"\t"<<dncep[1]<<endl;
	//	cout<<stop_time<<"\t"<<stop_time/86400<<"\t"<<data[count].decday<<"\t"<<count<<endl;

	//	if(sens) exit(0);

	//increment time
	stop_time += timeinc;
      } //end data loop

      if(year == year_last && mo == month_last){ //finish month off w/ NCEP data
	while(getline(ncep,ncepLine)){
	  dat<<ncepLine<<endl;
	  //for(i=0;i<nskip;i++) getline(ncep,ncepLine);	  
	}
      }
      ncep.close();
      dat.close();

    } //end month loop
  }//end year loop
  }//end RAD.DAT


  // ****************  SEMI-MONTHLY *********************** //
  if(do_flux){
  double semi_val[n_semi_len];
  int n_semi[n_semi_len],i;
  float time_mid,temp;
  bool first(1);
  count=0; //data stream counter

  //open output file
  string datname = folder + "/semimonthly.txt";
  cout<<datname<<endl;
  ofstream dat(datname.c_str());

  //write header
  dat<<"year time Rshort rain lai Babove FSW npp nep cflux cco2 tranp evap ch20 Wflux Hflux sensgc sensvc Tcanopy fsc stsc soilW soilT snowD snowT Lresp Rresp Gresp runoff albedo Rh gpp"<<endl;

  for(int year = year_first;year <= year_last; year++){
    int mstart(1),mstop(12);
    if(year==year_first) { 
      mstart=month_first;
      float day(data[0].decday);
      stop_time = (firstday(mstart,year)+firstday(mstart+1,year))/2; 
      if(day < stop_time){
	first = 1;
	//start in first half of month
	time_mid = (firstday(mstart,year)+stop_time)/2;
      } else {
	first = 0;
	//start in second half of month
	time_mid = (firstday(mstart+1,year)+stop_time)/2;
	stop_time = firstday(mstart+1,year);
      }
    }
    if(year==year_last)  mstop =month_last;
    cout<<year<<": months "<<mstart<<" "<<mstop<<endl;
    for(int mo = mstart;mo <= mstop; mo++){

      while(count < static_cast<int>(data.size())){
	if(stop_time > firstday(mo+1,year)) break; //end processing month

	//start new bin
	for(i=0;i<n_semi_len;i++){
	  semi_val[i] = 0.0;
	  n_semi[i]=0;
	}
	  
	
	//1/2 month loop
	cout<<data[count].decday<<"\t"<<data[count].year<<endl;
	while(data[count].decday <= stop_time && data[count].year == year){
	  //data available and has to be averaged
	  for(i=0;i<n_semi_len;i++){
	    switch(i){
	      case 0:
		temp = data[count].rshort;
		break;
	      case 1:
		temp = data[count].pcpg*31557600;  //convert from  kg/m2/s -> mm/yr
		break;
	      case 2:
		temp = data[count].lai;
		break;
	      case 3:
		temp = data[count].Babove;
		break;
	      case 4:
		temp = data[count].FSW;
		break;
	      case 5:
		temp = data[count].npp;
		break;
	      case 6:
		temp = data[count].nep;
		break;
	      case 7:
		temp = data[count].cflux;
		break;
	      case 8:
		temp = data[count].cco2;
		break;
	      case 9:
		temp = data[count].transp;
		break;
	      case 10:
		temp = data[count].evap;
		break;
	      case 11:
		temp = data[count].ch20;
		break;
	      case 12:
		temp = data[count].Wflux;
		break;
	      case 13:
		temp = data[count].Hflux;
		break;
	      case 14:
		temp = data[count].sensgc;
		break;
	      case 15:
		temp = data[count].sensvc;
		break;
	      case 16:
		temp = data[count].Tcanopy;
		break;
	      case 17:
		temp = data[count].fsc;
		break;
	      case 18:
		temp = data[count].stsc;
		break;
	      case 19:
		temp = data[count].soilW;
		break;
	      case 20:
		temp = data[count].soilT;
		break;
	      case 21:
		temp = data[count].snowD;
		break;
	      case 22:
		temp = data[count].snowT;
		break;
	      case 23:
		temp = data[count].Lresp;
		break;
	      case 24:
		temp = data[count].Rresp;
		break;
	      case 25:
		temp = data[count].Gresp;
		break;
	      case 26:
		temp = data[count].runoff;
		break;
	      case 27:
		temp = data[count].albedo;
		break;
	      case 28:
		temp = data[count].Rh;
		break;
	      case 29:
		temp = data[count].gpp;
		break;
	    }
	    if(temp > fNA){
	      semi_val[i] += temp;
	      n_semi[i]++;
	    }
	  }
	  count++;
	}
	
	//averaging
	for(i=0;i<n_semi_len;i++){
	  cout<<n_semi[i]<<"\t";
	  if(n_semi[i] > 0){
	    semi_val[i] = semi_val[i]/n_semi[i];
	  } else {semi_val[i] = fNA;}
	}
	cout<<endl;

	//write out value
	dat<<year<<"\t"<<time_mid/firstday(13,year);
	cout<<year<<"\t"<<time_mid<<"\t"<<stop_time<<"\t"<<firstday(mo+1,year)<<endl;
	for(i=0;i<n_semi_len;i++){
	  if(n_semi[i] > 0){
	    dat<<"\t"<<semi_val[i];
	  } else { dat<<"\tNA";
	  }
	}
	dat<<endl;

	//increment time
	if(first){
	  //move to second half of month
	  time_mid = (firstday(mo+1,year)+stop_time)/2;
	  stop_time = firstday(mo+1,year);
	  first = 0;
	} else {
	  //move to first half of next month
	  if(mo < 12){
	    stop_time = (firstday(mo+1,year)+firstday(mo+2,year))/2;
	    time_mid = (firstday(mo+1,year)+stop_time)/2;
	  } else {
	    stop_time = firstday(2,year+1)/2;
	    time_mid = stop_time/2;
	  }
	  first = 1;
	  break; //go to next month
	}
      } //end data loop

    } //end month loop
  }//end year loop
  dat.close();
  } //end empirical flux summary

}
