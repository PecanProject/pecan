met2CF.csv(in.path,in.file,outfolder){

  files = dir(in.path,in.file)
  files = files[grep("*.csv",files)]
  
  if(length(files)==0) return(NULL)
  
  for(i in 1:length(files)){
  
    ### if reading ameriflux .csv file ###
    dat <- read.csv("AMF_USMOz_2004_L2_WG_V004.csv",skip=17,na.strings=c(-9999,-6999)) #example file
    units <- dat[1,]
    dat <- dat[-1:-2,]
    dat$TA <- as.numeric(dat$TA)+273.15

    vars <- c("WS","TA","PRESS","Rg","Rgl","PREC")
    new.vars <- c("wind_speed","air_temperature","air_pressure",
              "surface_downwelling_shortwave_flux",
              "surface_downwelling_longwave_flux","precipitation_flux") #netCDF CF long variable names

    names(dat)[names(dat) %in% vars] <- new.vars #replace column names with netCDF CF names

    ncdim_def() #define netCDF dimensions for variables
    ncvar_def(name="wind_speed",units="m s-1",dim) #define netCDF variables
    nc_create("AMF_USMOz_L2_WG_02004.nc", vars=dat[new.vars]) #create netCDF file

  } ## end loop over files
    
}