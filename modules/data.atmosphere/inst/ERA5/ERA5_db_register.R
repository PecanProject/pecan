library(tidyverse)

ERA5.files.path <- "/fs/data1/pecan.data/input/ERA5_ENS"
#read the settings
settings <-PEcAn.settings::read.settings(file.choose())
#start the connection
con <- PEcAn.DB::db.open(settings$database$bety)

# Files name format in this folder needs to look like ERA5_1986.nc *** ERA5_(Year).nc
files <-list.files(ERA5.files.path, full.names = T)

#adding a record to the input table.
added<-PEcAn.DB::dbfile.input.insert(
  in.path=ERA5.files.path,
  in.prefix='ERA5_',
  siteid='1000026755', # This site USA
  startdate=as.Date(paste0(1986,"-01-01"), tz="UTC"),
  enddate=as.Date(paste0(2018,"-12-31"), tz="UTC"),
  mimetype="application/x-netcdf",
  formatname="CF Meteorology",
  parentid = NA,
  con,
  hostname = PEcAn.remote::fqdn(),
  allow.conflicting.dates = FALSE
)