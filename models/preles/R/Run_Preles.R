

runPreles.R <- (in.path, in.prefix, start_date, end_date,outdir, sitelat, sitelon,...){

require("Rpreles")
if(!require("Rpreles")) print("install Rpreles")
source("met2model.PRELES.R")
source("model2netcdf.PRELES.R")

met2model.PRELES(in.path,in.prefix,start_date,end_date)

preles_out=as.data.frame(PRELES(PAR=out[,"PAR"],TAir=out[,"TAir"],VPD=out[,"VPD",Precip=out[,"precip"],CO2=out[,"CO2"],fAPAR=out[,"fAPAR"]))

model2netcdf.PRELES(sitelat, sitelon, start_date, end_date)

}