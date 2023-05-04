library(tidyverse)

ERA5.files.path <- "/fs/data1/pecan.data/input/ERA5_ENS"
#read the settings
settings <-PEcAn.settings::read.settings(file.choose())
#start the connection
con <- PEcAn.DB::db.open(settings$database$bety)

#adding a record to the input table.
added<-PEcAn.DB::dbfile.input.insert(
  in.path=ERA5.files.path,
  in.prefix='ERA5_',
  siteid='1000026755', # This is site USA
  startdate=as.Date(paste0(1986,"-01-01"), tz="UTC"), # look into the files and make sure you have all the files for this time period
  enddate=as.Date(paste0(2018,"-12-31"), tz="UTC"),
  mimetype="application/x-netcdf",
  formatname="CF Meteorology",
  parentid = NA,
  con,
  hostname = PEcAn.remote::fqdn(),
  allow.conflicting.dates = FALSE
)
