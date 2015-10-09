

runPrles.R <- (in.path, in.prefix, start_date, end_date,outdir, sitelat, sitelon,...){

library(Rpreles)
require(ncdf4)
require(lubridate)
source("met2model.PRELES.R")
source("model2netcdf")

met2model.PRELES(in.path,in.prefix,start_dat,end_date)

preles_out=as.data.frame(Rpreles(PAR=out$PAR,TAir=TAir,VPD=out$VPD,Precip=out$precip,CO2=out$CO2,fAPAR=out$fAPAR))

model2netcdf.PRELES(sitelat, sitelon, start_date, end_date)


}