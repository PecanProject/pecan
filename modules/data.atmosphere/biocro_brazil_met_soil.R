## Originally names SimBrazil.r
## written by Deepak Jaiswal
## Contains code for handling both met NetCDF data and soils data

## This script has code to deal with both met and soil data

args <- commandArgs(TRUE)

if(length(args) == 0){
  warning("args is missing")
}else{
  k <- as.character(args)
}
library(XML)
input<-xmlParse(k)
settings<-xmlToList(input)

#####Getting Phenology Parameters ######
pheno<-settings$pft$phenoParms
pp<-phenoParms()
pp[names(pheno)]<-as.numeric(pheno)


######Getting Nitrogen Parameters #####
nitro<-settings$pft$nitroParms
nitroP<-nitroParms()
nitroP[names(nitro)]<-as.numeric(nitro)

### Getting Photosynthesis Parameters ####
photo<-settings$pft$photoParms
photoP<-photoParms()
photoP[names(photo)]<-as.numeric(photo)

### Getting Senescence Parameters ######
sene<-settings$pft$seneParms
senP<-seneParms()
senP[names(sene)]<-as.numeric(sene)

##### Getting Canopy Parameters #####
canopy<-settings$pft$canopyParms
canopyP40<-canopyParms()
## canopyP40[names(canopy)]<-as.numeric(canopy) # Error message Error: (list) object cannot be coerced to type 'double'
# read individual elements
canopyP40$Sp<-as.numeric(canopy$Sp)
canopyP40$SpD<-as.numeric(canopy$SpD)
canopyP40$nlayers<-as.numeric(canopy$nlayers)
canopyP40$chi.l<-as.numeric(canopy$chi.l)
canopyP40$heightFactor<-as.numeric(canopy$heightFactor)

# Reading sugar phenology
sugarpheno<-settings$pft$SugarPhenoParms
ppP50<-SugarPhenoParms()
ppP50[names(sugarpheno)]<-as.numeric(sugarpheno)

# Fetching Planting and Harvets Dates and converting them into Numeric
plantingdate<-settings$biocro_misc$dateofplanting
harvestdate<-settings$biocro_misc$dateofharvest

datetoDMY<-function(date){
  day<-as.numeric(substr(date,start=4,stop=5))
  month<-as.numeric(substr(date,start=1,stop=2))
  year<-as.numeric(substr(date,start=7,stop=10))
 list(day=day,month=month,year=year)
                        }



# function to check leap year; returns 1 if leap year;  0 if not leap year
isleapyear<-function(year){
  if(year%%100==0)
    {
    if(year%%400==0)test<-1
    }
  else
    {
      if(year%%4==0) {
                      test<-1
                    }
      else test<-0
    }
  test
}



# function to obtain day1 and dayn based on planting and harvest date
# input must be in the form of output generated from datetoDMY function
#returns a list with day1 and dayn

getday1dayn<-function(pdate,hdate)
  {
    
    d1<-pdate$day
    m1<-pdate$month
    y1<-pdate$year
    dn<-hdate$day
    mn<-hdate$month
    yn<-hdate$year

   # calculating day lost in planting year

   # get number of days from date of planting and beginning of year
      if((isleapyear(y1))==1)
        days<-c(31,29,31,30,31,30,31,31,30,31,30,31)
      if(!(isleapyear(y1))==1)
       days<-c(31,28,31,30,31,30,31,31,30,31,30,31)
      if(m1==1)
        doy1<-pdate$day
      else
       doy1<-sum(days[1:(m1-1)])+pdate$day

    # no of days between day of harvesting and 1st day of year
       if((isleapyear(yn))==1)
        days<-c(31,29,31,30,31,30,31,31,30,31,30,31)
      if(!(isleapyear(yn))==1)
       days<-c(31,28,31,30,31,30,31,31,30,31,30,31)
      if(mn==1)
        doyn<-hdate$day
      else
       doyn<-sum(days[1:(mn-1)])+hdate$day

   if(y1==yn)    #case2 when planting and harvest takes place in the same year
      {
       doy1<-doy1
       doyn<-doyn
      }
    else    #case2 when planting and harvest do not take place in the same year
      {
        dayn<-0
       for (i in y1:(yn-1)){
                            if (isleapyear(i)==1)
                           days<-sum(c(31,29,31,30,31,30,31,31,30,31,30,31))
                           if (!(isleapyear(i)==1))
                           days<-sum(c(31,28,31,30,31,30,31,31,30,31,30,31))
                           dayn<-dayn+days
                          }
        doyn=dayn+doyn
        doy1=doy1
     }
     list(day1=doy1,dayn=doyn)
  }

pdate<-datetoDMY(plantingdate)
hdate<-datetoDMY(harvestdate)

tmp<-getday1dayn(pdate,hdate)
day1<-tmp$day1
dayn<-tmp$dayn


library(ncdf)


#get how many years are needed in the 4th dimension of climate data to aggregate

dimyr<-(hdate$year-pdate$year)+1
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
iii<-as.numeric(pdate$year-1) #initialize iii to read appropriate climate data file in the next for loop
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
                              
                             }
                           }
                     }
          # remove unnecessary  R objects. They are recycled in the loop if planting and harvesting takes place in different year
             remove(shum,temp,wind,solar,precip,tempmin,tempmax)           
                  }

##Note that spatial range of NCP data is 0 to 360 East
#Which Means 0 to 180 are represent correctly Eastwards and 180 to 360 East simply represents -180 to 0 West
#However, in order to interpret -180 to -360 (w.r.t google, for e.g) we must make following changes
# Lon=Lon+360, so -360 is represented by 0 and -330E is represented by +30E
             tmp0<-open.ncdf(paste("/home/djaiswal/database/NCEP/Precipitation/prate.sfc.gauss.",iii,".nc",sep=""))
             Lat<-get.var.ncdf(tmp0,"lat")
             Lon<-get.var.ncdf(tmp0,"lon")
             close.ncdf(tmp0)
             doy<-seq(1,365)
for (i in 1:length(Lon))
     {
     if(Lon[i]> 180)
       {
       Lon[i]<-Lon[i]-360
     }
   }
    



#read Brazil soil data after processing to take care of NA
# see rscript /home/djaiswal/database/BrazilSoil/soildataprocessing.R

soils<-read.csv("/home/djaiswal/database/BrazilSoil/processedsoildatafromBrazil.csv")
check<-rep(0,length(soils$SerialNumber)) # this new colum is to check whether calculations have been performed at current location to avoid duplicaion of calculation
ii<-rep(0,length(soils$SerialNumber)) # to keep track of ii or Longitude index corresponding to a soil row
jj<-rep(0,length(soils$SerialNumber)) # to keep track of jj or Latitude index corresponding to a soil row
soils<-cbind(soils,check,ii,jj)  # combine check column with soil, check=0 represents that at this location calculation has not yet been performed

# To get rid of factor(type vs numeric), if any
soils$SoilDepth<-as.numeric(soils$SoilDepth)
soils$Sand<-as.numeric(soils$Sand)
soils$Silt<-as.numeric(soils$Silt)
soils$Clay<-as.numeric(soils$Clay)
soils$OM<-as.numeric(soils$OM)

# Start Calculation with soil database
 for (isoil in 1:nrow(soils))
               {
                 soillat<-soils[isoil,]$Lat
                 soillon<-soils[isoil,]$Long
                 SerialID<-soils[isoil,]$SerialNUmber
                 for (ii in 1:192) # Longitude Index
                   {
                     for (jj in 1:94) # Latitude Index
                       {
                         currentlon<-Lon[ii]
                         currentlat<-Lat[jj]
                      if((abs(soillat-currentlat))<=2.0)
                       {
                        if((abs(soillon-currentlon))<=2.0)
                          {
                          # Now we have located a soil type for this latitude and longitude
                           # read soil information
                           soils[isoil,]$ii<-ii
                           soils[isoil,]$jj<-jj
                           soils[isoil,]$check=1
                           ddepth<-soils[isoil,]$SoilDepth
                           LLat<-soils[isoil,]$Lat
                           LLong<-soils[isoil,]$Long
                           SSilt<-soils[isoil,]$Silt
                           CClay<-soils[isoil,]$Clay
                           SSand<-soils[isoil,]$Sand
                           OOM<-soils[isoil,]$OM

                                weachyear<-numeric(0)
                                weachday<-numeric(0)
                                weachhumidity<-numeric(0)
                                weachsolar<-numeric(0)
                                weachtemp<-numeric(0)
                                weachwind<-numeric(0)
                                weachprecip<-numeric(0)
                                weachtempmax<-numeric(0)
                                weachtempmin<-numeric(0)

                                 dimyr<-(hdate$year-pdate$year)+1
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
                   forweach<-data.frame(year=weachyear,day=weachday,solarR=weachsolar,Tmax=weachtempmax,Tmin= weachtempmin,Tavg= weachtemp,RHmax=relativehumiditymax,RHmin=relativehumiditymin,RHavg=relativehumidity,WS= weachwind,precip= weachprecip)
                    #call weachNEW
                    wet<-weachNEW(forweach,lat=-23,ts=1, temp.units="Celsius", rh.units="fraction",ws.units="mph",pp.units="in")
                    soilP<-soilParms(wsFun="linear",phi2=0.83,rfl=1.7, soilType=3,soilLayers=10,soilDepth=ddepth,optiontocalculaterootdepth=0,rootfrontvelocity=0.5,FieldC=0.15,WiltP=0.083)
           res<- BioGro(wet,lat=soillat,phenoControl=pp,canopyControl=canopyP40, soilControl=soilP,photoControl=photoP, seneControl=senP,sugarphenoControl=ppP50,nitroControl=nitroP,day1 = day1, dayn = dayn,iRhizome=4,irtl=1e-5)
                            Stem<-res$Stem[length(res$Stem)]
                            Root<-res$Root[length(res$Root)]
                            Leaf<-res$Leaf[length(res$Leaf)]
                            LAI<-res$LAI[length(res$LAI)]
                           ID<-soils[isoil,]$SerialNumber
                           datatowrite<-data.frame(ID=ID,Stem=Stem,Leaf=Leaf,Root=Root,LAI=LAI)
                    write.table(datatowrite,file=paste(k,".results.txt",sep=""),append=TRUE,row.names=FALSE,col.names=FALSE)
                           datatowrite<-NULL
                         }# longitude check
                        } #latitude check                       
                       }#latitude index
                   } #longitude index
               }# This is end of soil loop

            
         
                           
                        

     

                         
