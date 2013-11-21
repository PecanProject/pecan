library(BioCro)
library(XML)
#############Reading from the fXML file generated via web-interface######
settings.xml<-xmlParse('./config.xml')
settings <- xmlToList(settings.xml)

################ Read  Year and Location Details and derive model input############
lat<-as.numeric(settings$location$latitude)
lon<-as.numeric(settings$location$longitude)
dateofplanting<-settings$simulationPeriod$dateofplanting
dateofharvest<-settings$simulationPeriod$dateofharvest
year1<-as.numeric(substr(dateofplanting,7,10))
date1<-as.numeric(substr(dateofplanting,4,5))
month1<-as.numeric(substr(dateofplanting,1,2))
year2<-as.numeric(substr(dateofharvest,7,10))
date2<-as.numeric(substr(dateofharvest,4,5))
month2<-as.numeric(substr(dateofharvest,1,2))

dummydate<-as.Date(c(paste(year1,"-1","-1",sep=""),paste(year1,"-",month1,"-",date1,sep=""),paste(year2,"-",month2,"-",date2,sep="")))
day1<-as.numeric(dummydate[2]-dummydate[1])
dayn<-as.numeric(dummydate[3]-dummydate[1])

#####Getting Phenology Parameters #######################################


pheno<-settings$pft$phenoParms
pp<-phenoParms()
pp[names(pheno)]<-as.numeric(pheno)


######Getting Nitrogen Parameters #######################################
nitro<-settings$pft$nitroParms
nitroP<-nitroParms()
nitroP[names(nitro)]<-as.numeric(nitro)

### Getting Photosynthesis Parameters ####################################
photo<-settings$pft$photoParms
photoP<-photoParms()
photoP[names(photo)]<-as.numeric(photo)

### Getting Senescence Parameters ###############################################
sene<-settings$pft$seneParms
senP<-seneParms()
senP[names(sene)]<-as.numeric(sene)

#### Getting Soil Parameters##################################################
soil<-settings$pft$soilParms
soilP<-soilParms()
soilP[names(soil)]<-as.numeric(soil)

##### Getting Canopy Parameters #######################################
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

##Here we are extracting weather data from NCEP and preparing for model #######

#Declare a function
library(RNCEP) 
 InputForWeach<-function (lat,lon,year1,year2){
  
    # Get  Temperature Records
     avgTemp<-NCEP.gather(variable=c('air.2m'), level='gaussian', months.minmax=c(1,12), years.minmax=c(year1,year2),lat.southnorth=c(lat,lat),lon.westeast=c(lon,lon), reanalysis2 = TRUE,
    return.units = FALSE, status.bar=FALSE)
    avgTemp<-NCEP.aggregate(avgTemp,HOURS=FALSE,fxn='mean')  # Average Flux for the whole day
    avgTemp<-NCEP.array2df(avgTemp,var.name='avgTemp')
    avgTemp<-aggregate(avgTemp~datetime,data=avgTemp,mean) # mean of all nearby spatial locations
    avgTemp$datetime<-substr(avgTemp$datetime,1,10)
   avgTemp$avgTemp<-avgTemp$avgTemp-273  

      # Get Solar Radiation Records
      solarR<-NCEP.gather(variable=c('dswrf.sfc'), level='gaussian', months.minmax=c(1,12), years.minmax=c(year1,year2),lat.southnorth=c(lat,lat),lon.westeast=c(lon,lon), reanalysis2 = TRUE,
    return.units = FALSE, status.bar=FALSE)
    solarR<-NCEP.aggregate(solarR,HOURS=FALSE,fxn='mean')  # Average Flux for the whole day 
    solarR<-NCEP.array2df(solarR,var.name='solarR')
    solarR$solarR<- solarR$solarR*24*60*60*1e-6 # To convert Wt/m2 to MJ/m2
    solarR<-aggregate(solarR~datetime,data=solarR,mean) # mean of all nearby spatial locations
    solarR$datetime<-substr(solarR$datetime,1,10)

     ## T Maximum Data
     Tmax<-NCEP.gather(variable=c('tmax.2m'), level='gaussian', months.minmax=c(1,12), years.minmax=c(year1,year2),lat.southnorth=c(lat,lat),lon.westeast=c(lon,lon), reanalysis2 = TRUE,
    return.units = FALSE, status.bar=FALSE)
    Tmax<-NCEP.aggregate(Tmax,HOURS=FALSE,fxn='max')
    Tmax<-NCEP.array2df(Tmax,var.name='Tmax')
    Tmax<-aggregate(Tmax~datetime,data=Tmax,max)
    Tmax$datetime<-substr(Tmax$datetime,1,10)
    Tmax$Tmax<-Tmax$Tmax-273  


     ## T Minimum Data
     Tmin<-NCEP.gather(variable=c('tmin.2m'), level='gaussian', months.minmax=c(1,12), years.minmax=c(year1,year2),lat.southnorth=c(lat,lat),lon.westeast=c(lon,lon), reanalysis2 = TRUE,
    return.units = FALSE, status.bar=FALSE)
    Tmin<-NCEP.aggregate(Tmin,HOURS=FALSE,fxn='max')
    Tmin<-NCEP.array2df(Tmin,var.name='Tmin')
    Tmin<-aggregate(Tmin~datetime,data=Tmin,max)
    Tmin$datetime<-substr(Tmin$datetime,1,10)
    Tmin$Tmin<-Tmin$Tmin-273  

  ## Relative Humidity (I am using surface level, not Grid level to get rlative humidity, not absolute humidity, hope its not a problem. 
    RH<-NCEP.gather(variable=c('rhum.sig995'), level='surface', months.minmax=c(1,12), years.minmax=c(year1,year2),lat.southnorth=c(lat,lat),lon.westeast=c(lon,lon), reanalysis2 = TRUE,return.units = FALSE, status.bar=FALSE)

     # Warnign Message, not available in Reanalysis 2, Instead using Reanalysis 1.
     
    RHavg<-NCEP.aggregate(RH,HOURS=FALSE,fxn='mean')
    RHmax<-NCEP.aggregate(RH,HOURS=FALSE,fxn='max')
    RHmin<-NCEP.aggregate(RH,HOURS=FALSE,fxn='min')
    RHavg<-NCEP.array2df(RHavg,var.name='RH')
    RHmax<-NCEP.array2df(RHmax,var.name='RH')
    RHmin<-NCEP.array2df(RHmin,var.name='RH')
     
    RHavg<-aggregate(RH~datetime,data=RHavg,mean)
    RHmax<-aggregate(RH~datetime,data=RHmax,max)
    RHmin<-aggregate(RH~datetime,data=RHmin,min)
    RHavg$datetime<-substr(RHavg$datetime,1,10)
    RHmax$datetime<-substr(RHmax$datetime,1,10)
    RHmin$datetime<-substr(RHmin$datetime,1,10)
    RHavg$RH<-RHavg$RH*0.01    ## Percent to Fraction
    RHmax$RH<-RHmax$RH*0.01  ## Percent to Fraction
    RHmin$RH<-RHmin$RH*0.01   ## Percent to Fraction
 

     ## WInD SPEED

     Vwind<-NCEP.gather(variable=c('vwnd.10m'), level='gaussian', months.minmax=c(1,12), years.minmax=c(year1,year2),lat.southnorth=c(lat,lat),lon.westeast=c(lon,lon), reanalysis2 = TRUE,
    return.units = FALSE, status.bar=FALSE
    Vwind<-NCEP.aggregate(Vwind,HOURS=FALSE,fxn='mean')
    Vwind<-NCEP.array2df(Vwind,var.name='Vwind')
    Vwind<-aggregate(Vwind~datetime,data=Vwind,mean)
    Vwind$datetime<-substr(Vwind$datetime,1,10)
    
 Uwind<-NCEP.gather(variable=c('uwnd.10m'), level='gaussian', months.minmax=c(1,12), years.minmax=c(year1,year2),lat.southnorth=c(lat,lat),lon.westeast=c(lon,lon), reanalysis2 = TRUE,
    return.units = FALSE, status.bar=FALSE)
    Uwind<-NCEP.aggregate(Uwind,HOURS=FALSE,fxn='mean')
    Uwind<-NCEP.array2df(Uwind,var.name='Uwind')
    Uwind<-aggregate(Uwind~datetime,data=Uwind,mean)
    Uwind$datetime<-substr(Uwind$datetime,1,10)
 
     Uwind$Uwind<-sqrt(Uwind$Uwind^2+Vwind$Vwind^2)
     Uwind$Uwind<-Uwind$Uwind*4.87/log(67.8*10-5.42) # converting Windspeed from 10m to 2 m height using correlation provided by FAO (http://www.fao.org/docrep/X0490E/x0490e07.htm)
     
   Uwind$Uwind<-Uwind$Uwind*(3600)/(1609)   # unit conversion from m/s to miles per hr

     
 ## Precipitation
     
     Rain<-NCEP.gather(variable=c('prate.sfc'), level='gaussian', months.minmax=c(1,12), years.minmax=c(year1,year2),lat.southnorth=c(lat,lat),lon.westeast=c(lon,lon), reanalysis2 = TRUE,
    return.units = FALSE, status.bar=FALSE)
    Rain<-NCEP.aggregate(Rain,HOURS=FALSE,fxn='mean')
    Rain<-NCEP.array2df(Rain,var.name='Rain')
    Rain<-aggregate(Rain~datetime,data=Rain,mean)
    Rain$datetime<-substr(Rain$datetime,1,10)
    Rain$Rain<-Rain$Rain*(24*60*60)*(1/1000)*39.37   # Converting from kg/m2 sec to kg/m2 to m3/m2 to inches

      day<-numeric(0)
     year<-numeric(0)
for (i in year1:year2)
  {
     if((i%%400)==0 || (i%%100!=0 && i%%4==0))
     {
       indx<-as.integer(length(day))
       day[as.integer(indx+1):as.integer(indx+366)]=seq(1:366)
       year[as.integer(indx+1):as.integer(indx+366)]=rep(i,366)
     }
      if(!((i%%400)==0 || (i%%100!=0 && i%%4==0)))
        {
       indx<-as.integer(length(day))
       day[as.integer(indx+1):as.integer(indx+365)]=seq(1:365)
       year[as.integer(indx+1):as.integer(indx+365)]=rep(i,365)
       }
   }
result<-data.frame(year=year,day=day,solarR=solarR$solarR,Tmax=Tmax$Tmax, Tmin=Tmin$Tmin, Tavg=avgTemp$avgTemp, RHmax=RHmax$RH,RHmin=RHmin$RH, RHavg=RHavg$RH, WS=Uwind$Uwind,precip=Rain$Rain)
     return(result)
   }

climdata<-InputForWeach(lat=lat,lon=lon,year1=year1,year2=year2)
wet<-weachNEW(climdata,lat=lat,ts=1, temp.units="Celsius", rh.units="fraction",ws.units="mph",pp.units="in")


res<- BioGro(wet,lat=lat,phenoControl=pp,canopyControl=canopyP40, soilControl=soilP,photoControl=photoP, seneControl=senP,sugarphenoControl=ppP50,nitroControl=nitroP,day1 = day1, dayn = dayn,iRhizome=4,irtl=1e-03)

Date<-seq(dummydate[2],dummydate[3]+1,1)
DAP<-seq(0,length(Date)-1)
Leaf=res$Leaf
Stem=res$Stem
Root=res$Root
Sugar<-res$Sugar
LAI<-res$LAI
ThermalT<-res$dailyThermalT

toprint<-data.frame(DAP=DAP,Leaf=Leaf,Stem=Stem,Root=Root,Sugar=Sugar,LAI=LAI,ThermalT=ThermalT)

round(toprint,digit=2)
write.table(toprint, file="./ooutput",append=FALSE,sep='\t')
