##' Get Met data from NCEP
##'
##' Retrieves NCEP met data for specified lat x lon and time span
##' @title get NCEP met
##' @param lat 
##' @param lon 
##' @param start.date 
##' @param end.date 
##' @param site.id 
##' @param con
##' @export
##' @return weather data frame with weather formatted for BioCro model
##' @author Deepak Jaiswal, David LeBauer
get.ncepmet <- function(){
  years <- sequence(from = year(start.date), to = year(end.date))
  
  pdate<-datetoDMY(paste0("05/01/", yeari))
  hdate<-datetoDMY(paste0("12/01/", yeari))
  

  
  
  library(ncdf)
  
  
  #get how many years are needed in the 4th dimension of climate data to aggregate
  
  dimyr<-length(years)
  #this variable is to track year's ID index in multidimensional array
  iyear<-0
  #declare arrays for each of the five variables needed (ignore 366 days of leap year)
  shum4d<-array(0,dim=c(192,94,366,dimyr))
  solar4d<-array(0,dim=c(192,94,366,dimyr))
  temp4d<-array(0,dim=c(192,94,366,dimyr))
  tempmin4d<-array(0,dim=c(192,94,366,dimyr))
  tempmax4d<-array(0,dim=c(192,94,366,dimyr))
  wind4d<-array(0,dim=c(192,94,366,dimyr))
  precip4d<-array(0,dim=c(192,94,366,dimyr))
  soilm.0.10.4d<-array(0,dim=c(192,94,366,dimyr))
  soilm.10.200.4d<-array(0,dim=c(192,94,366,dimyr))
  iii<-as.numeric(min(years)) #initialize iii to read appropriate climate data file in the next for loop
  for (i in 1:dimyr)
  {
    
    #declaring array to read netcdf files
    shum<-array(0,dim=c(192,94,366))
    solar<-array(0,dim=c(192,94,366))
    temp<-array(0,dim=c(192,94,366))
    tempmin<-array(0,dim=c(192,94,366))
    tempmax<-array(0,dim=c(192,94,366))
    wind<-array(0,dim=c(192,94,366))
    precip<-array(0,dim=c(192,94,366))
    soilm.0.10<-array(0,dim=c(192,94,366))
    soilm.10.200<-array(0,dim=c(192,94,366))
    
    iii<-iii+1 
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/SpecificHumidity/shum.2m.gauss.",iii,".nc",sep=""))
    shum<-get.var.ncdf(tmp0)
    close.ncdf(tmp0)
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/Temperature/air.2m.gauss.",iii,".nc",sep=""))
    temp<-get.var.ncdf(tmp0)
    close.ncdf(tmp0)
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/MinTemperature/tmin.2m.gauss.",iii,".nc",sep=""))
    tempmin<-get.var.ncdf(tmp0)
    close.ncdf(tmp0)
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/MaxTemperature/tmax.2m.gauss.",iii,".nc",sep=""))
    tempmax<-get.var.ncdf(tmp0)
    close.ncdf(tmp0)
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/WindspeedU/uwnd.10m.gauss.",iii,".nc",sep=""))
    wind<-get.var.ncdf(tmp0)
    close.ncdf(tmp0)
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/SolarRadiation/dswrf.sfc.gauss.",iii,".nc",sep=""))
    solar<-get.var.ncdf(tmp0)
    close.ncdf(tmp0)
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/Precipitation/prate.sfc.gauss.",iii,".nc",sep=""))
    precip<-get.var.ncdf(tmp0)
    Lat<-get.var.ncdf(tmp0,"lat")
    Lon<-get.var.ncdf(tmp0,"lon")
    close.ncdf(tmp0)
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/soilmoisture-0-10cm/soilw.0-10cm.gauss.",iii,".nc",sep=""))
    soilm.0.10<-get.var.ncdf(tmp0)
    close.ncdf(tmp0)
    tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/soilmoisture-10-200cm/soilw.10-200cm.gauss.",iii,".nc",sep=""))
    soilm.10.200<-get.var.ncdf(tmp0)
    close.ncdf(tmp0)
    iyear<-iyear+1 # year index is outside tripple for loop because it is controlled by outside for loop [i]
    for (ii in 1:192) #longitude index
    {
      for(jj in 1:94)#latitude index
      {
        for(kk in 1:365)# day index (ignoring leap year for now)
        {
          shum4d[ii,jj,kk,iyear]=shum[ii,jj,kk]
          temp4d[ii,jj,kk,iyear]=temp[ii,jj,kk]
          tempmin4d[ii,jj,kk,iyear]=tempmin[ii,jj,kk]
          tempmax4d[ii,jj,kk,iyear]=tempmax[ii,jj,kk]
          wind4d[ii,jj,kk,iyear]=wind[ii,jj,kk]
          solar4d[ii,jj,kk,iyear]=solar[ii,jj,kk]
          precip4d[ii,jj,kk,iyear]=precip[ii,jj,kk]
          soilm.0.10.4d[ii,jj,kk,iyear]=soilm.0.10[ii,jj,kk]
          soilm.10.200.4d[ii,jj,kk,iyear]=soilm.10.200[ii,jj,kk]
        }
      }
    }     
  }
    
  
  # Start  The Global Loop
  
  for (ii in 1:192) # Chaange it back to 1 to 192
  {
    for (jj in 1:94)# 94 # Latitude Index
    {
      currentlon<-Lon[ii]
      currentlat<-Lat[jj]
      # Initialization and parameters for the new grid point
      
      ## Correcting for Lon scale of NCEP (0 to 360) versus Soil Database (-180 to 180)i
      if(currentlon > 180)
      {
        currentlon<-currentlon-360
      }
      
      
      ## reading soil database for the current climate grid
      gridsize = 1.9
        
        weachyear<-numeric(0)
        weachday<-numeric(0)
        weachhumidity<-numeric(0)
        weachsolar<-numeric(0)
        weachtemp<-numeric(0)
        weachwind<-numeric(0)
        weachprecip<-numeric(0)
        weachtempmax<-numeric(0)
        weachtempmin<-numeric(0)
        
        dimyr<-length(years)
        for (i in 1:dimyr) # this loop will go multiple times depending on diff betwn harvest and plant
        {
          iweachyr<-pdate$year+i-1
          weachyear<-as.vector(c(weachyear,rep(iweachyr,365)))
          weachday<-as.vector(c(weachday,seq(1,365)))
          # get year index for 4D climdate data,which starts from 1 to number of years
          # i also works for the index of 4D array containing data
          weachtemp<-as.vector(c(weachtemp,temp4d[ii,jj,1:365,i]))
          weachtempmax<-as.vector(c(weachtempmax,tempmax4d[ii,jj,1:365,i]))
          weachtempmin<-as.vector(c(weachtempmin,tempmin4d[ii,jj,1:365,i]))
          weachhumidity<-as.vector(c(weachhumidity,shum4d[ii,jj,1:365,i]))
          weachsolar<-as.vector(c(weachsolar,solar4d[ii,jj,1:365,i]))
          weachwind<-as.vector(c(weachwind,wind4d[ii,jj,1:365,i]))
          weachprecip<-as.vector(c(weachprecip,precip4d[ii,jj,1:365,i]))
          
          #here I am simply adding one extra row for leap year to avoid probllem with newWEACH
          if(isleapyear(iweachyr)==1)
          {
            weachyear<-c(weachyear,iweachyr)
            weachday<-c(weachday,366)
            weachtemp<-c(weachtemp,temp4d[ii,jj,365,i]) # simply copy value from last day
            weachtempmax<-c(weachtempmax,tempmax4d[ii,jj,365,i])
            weachtempmin<-c(weachtempmin,tempmin4d[ii,jj,365,i])
            weachhumidity<-c(weachhumidity,shum4d[ii,jj,365,i])
            weachsolar<-c(weachsolar,solar4d[ii,jj,365,i])
            weachwind<-c(weachwind,wind4d[ii,jj,365,i])
            weachprecip<-c(weachprecip,precip4d[ii,jj,365,i]) 
          }
        }
        # Now we have extracted all the variables needed
        #but
        # unit conversion is needed
        weachtemp<-weachtemp-273    # kelvin to Centigrade
        weachtempmax<-weachtempmax-273  # kelvin to centigrade
        weachtempmin<-weachtempmin-273  # kelvin to Centigrade
        
        weachsolar<-weachsolar*24*60*60*1e-6 # W/m2 to MJ/m2
        weachwind<-abs(weachwind*3.85) # wind speed from 10m to 2m an dm/2 to miles/h
        weachprecip<-weachprecip*(24*60*60)*(1/1000)*39.37 #conerting kg/m2sec to kg.m2 to inches
        ######################################################################
        # converting specific humidity into relative humidity (surface flux data does not have RH
        # reference for calculation "The Rotronic Humidity HandBook"
        mixingratio<-weachhumidity/(1-weachhumidity)*1000
        # using mixing ratio and atm pressure, find partial press of water in pascal
        waterpartialpress<-(1e+5)*mixingratio/(621.9+mixingratio) # in pascal
        # saturated water vapor pressure using antoine equation
        satwatervappressmax<-(8.07131-(1730.63/(244.485+weachtempmax)))*133 # in pascal
        relativehumiditymin<-(waterpartialpress)/(satwatervappressmax) #RH fraction
        satwatervappressmin<-(8.07131-(1730.63/(244.485+weachtempmin)))*133 # in pascal
        relativehumiditymax<-(waterpartialpress)/(satwatervappressmin)#RH fraction
        relativehumidity<-0.5*( relativehumiditymax+relativehumiditymin)
        
        # getting rid of missing/strange values (very small negative) of other troublesome climate data to avoid error in simulation
        weachsolar<-ifelse(weachsolar>32765,30,weachsolar)
        weachsolar<-ifelse(weachsolar<0,0,weachsolar)
        weachtempmax<-ifelse(weachtempmax>32765,30,weachtempmax)
        weachtempmax<-ifelse(weachtempmax<0,0,weachtempmax)
        weachtempmin<-ifelse(weachtempmin>32765,30,weachtempmin)
        weachtempmin<-ifelse(weachtempmin<0,0,weachtempmin)
        weachtemp<-ifelse(weachtemp>32765,30,weachtemp)
        weachtemp<-ifelse(weachtemp<0,0,weachtemp)
        relativehumiditymin<-ifelse(relativehumiditymin>1,0.6,relativehumiditymin) # using 1 here for now, later chekc from NCEP website whats being used for missing value
        relativehumiditymin<-ifelse(relativehumiditymin<0,0.1,relativehumiditymin)
        relativehumiditymax<-ifelse(relativehumiditymax>1,0.6,relativehumiditymax) # using 1 now
        relativehumiditymax<-ifelse(relativehumiditymax<0,0.1,relativehumiditymax)
        relativehumidity<-ifelse(relativehumidity>1,0.6,relativehumidity) #using 1 now
        relativehumidity<-ifelse(relativehumidity<0,0.1,relativehumidity)
        weachwind<-ifelse(weachwind>32765,30,weachwind)
        weachwind<-ifelse(weachwind<0,2,weachwind)
        weachprecip<-ifelse(weachprecip>32765,0,weachprecip)
        weachprecip<-ifelse(weachprecip<0,0,weachprecip)
        
        
        #Now we are ready to make dataframe to call weach
        forweach<-data.frame(year=weachyear,day=weachday,solarR=weachsolar,Tmax=weachtempmax,Tmin= weachtempmin,Tavg= weachtemp,RHmax=relativehumiditymax,RHmin=relativehumiditymin,
                             RHavg=relativehumidity,WS= weachwind,precip= weachprecip)

        #call weachNEW
        dat<-weachNEW(forweach,lat=currentlat,ts=1, temp.units="Celsius", rh.units="fraction",ws.units="mph",pp.units="in")
  
  
  
}