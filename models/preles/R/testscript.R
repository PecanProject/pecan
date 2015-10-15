library("ncdf4")
library("ncdf4.helpers")
library("Rpreles")
library("udunits2")
library("PEcAn.utils")

nc=nc_open("US-WCr.2005.nc")
dim=nc.get.dim.names(nc)
vars=nc.get.variable.list(nc)

lat ="45.0060"
lon = "-90.0798"
PAR <-ncvar_get(nc,"PAR")
Tair <-ncvar_get(nc,"TA")
Precip <-ncvar_get(nc,"PREC")
VPD <-ncvar_get(nc,"VPD")
CO2 <-ncvar_get(nc,"CO2")
doy <-ncvar_get(nc,"DOY")

PAR= tapply(PAR, doy,sum,na.rm=TRUE)
TAir=tapply (Tair,doy,mean,na.rm=TRUE)
VPD= udunits2::ud.convert(tapply(VPD,doy,mean,na.rm=TRUE), "Pa","kPa")
Precip=tapply(Precip,doy,sum, na.rm=TRUE)
CO2= tapply(CO2,doy,mean)
doy=tapply(doy,doy,mean)
fAPAR =rep (0.8,length=length(doy))

tmp<-cbind (PAR,TAir,VPD,Precip,CO2,fAPAR)

preles_out=as.data.frame(PRELES(PAR=tmp[,"PAR"],TAir=tmp[,"TAir"],VPD=tmp[,"VPD"], Precip=tmp[,"Precip"],CO2=tmp[,"CO2"],fAPAR=tmp[,"fAPAR"]))

PRELES.output<-preles_out
PRELES.output.dims<-dim(PRELES.output)

start_date="2005-01-01"
start_date<-as.POSIXlt(start_date,tz="GMT")
end_date="2005-12-31"
end_dat<-as.POSIXlt(end_date,tz ="GMT")

days=as.Date(start_date):as.Date(end_date)
year = strftime(as.Date(days,origin="1970-01-01","%Y"))
num.years<- length(unique(year))
years<-unique(year)
timestep.s<-86400
outdir= "."
y=years
for (y in years){
  if(file.exists(file.path(outdir,paste(y))))
    next
print(paste("----Processing year: ",y))

sub.PRELES.output<- subset(PRELES.output, years == y)
sub.PRELES.output.dims <- dim(sub.PRELES.output)

output <- list()
output[[1]] <- (sub.PRELES.output[1]*0.001)/timestep.s
output[[2]] <- (sub.PRELES.output[2]*0.001)/timestep.s
output[[3]] <- (sub.PRELES.output[3]*0.001)/timestep.s
output[[4]] <- (sub.PRELES.output[4])/timestep.s
output[[5]] <- (sub.PRELES.output[5])/timestep.s
output[[6]] <- (sub.PRELES.output[6]*0.001)/timestep.s
output[[7]] <- (sub.PRELES.output[7]*0.001)/timestep.s

sitelon=lon
sitelat=lat
t<- ncdim_def(name = "time",
              units =paste0("days since",y,"-01-01 00:00:00"),
              vals = 1:nrow(sub.PRELES.output),
              calendar = "standard",unlim =TRUE)
lat<- ncdim_def("lat", "degrees_east",
                vals=as.numeric(sitelat),
                longname = "station_longitude")
lon<- ncdim_def("lat", "degrees_northt",
                vals=as.numeric(sitelon),
                longname = "station_longitude")

for(i in 1:length(output)){
  if(length(output[[i]])==0)output[[i]]<- rep(-999,length(t$vals))
}

var<-list()
var[[1]]<- ncvar_def("GPP","kg/m2/s",list(lat,lon,t),NA)
var[[2]]<- ncvar_def("Evapotranspiration", "kg/m2/s",list(lon,lat,t), -999)
var[[3]]<- ncvar_def("SoilMoist","kg/m2", list(lat,lon,t),NA)
var[[4]]<- ncvar_def("fWE","NA",list(lon,lat,t),-999)
var[[5]]<- ncvar_def("fW","NA",list(lon,lat,t),-999)
var[[6]]<- ncvar_def("Evap","kg/m2/s", list(lon,lat,t),-999)
var[[7]]<- ncvar_def("TVeg","kg/m2/s",list(lon,lat,t),-999)

nc <-nc_create(file.path(outdir,paste(y,"nc", sep=".")),var)
varfile <- file(file.path(outdir,paste("2005","nc","var",sep=".")),"w")
for(i in 1:length(var)){
  ncvar_put(nc,var[[i]],output[[i]])
  cat(paste(var[[i]]$name,var[[i]]$longname),file=varfile,sep="/n")
}
close(varfile)
nc_close(nc)
}
