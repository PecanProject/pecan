met2cf.NARR <- function(outfolder,start_year,end_year){

# Defaults
start_year <- 1979 
end_year <- 2013
outfolder <- "/projectnb/cheas/pecan.data/input/NARR/"


vlist <- c("pres.sfc", "dswrf", "dlwrf", "air.2m", "shum.2m", "prate","vwnd.10m","uwnd.10m")

# system(paste("module load netcdf"))

# Get original Data
for (v in vlist){
  for (year in seq(end_year,start_year,by=-1)){
    system(paste("wget -c -P ", outfolder ," ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/",v,".", year,".nc",sep=""))
  }    
}

# system(paste("cd ", outfolder))
# system(paste("/projectnb/cheas/gapmacro/NARR/NewNARR/nc_formatting.sh"))
}


