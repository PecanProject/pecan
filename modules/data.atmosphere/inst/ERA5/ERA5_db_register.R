library(tidyverse)

ERA5.files.path <- "/fs/data1/pecan.data/input/ERA5_ENS"
#read the settings
settings <-PEcAn.settings::read.settings(file.choose())
#start the connection
con <- PEcAn.DB::db.open(settings$database$bety)

# Files name format in this folder needs to look like ERA5_1986.nc *** ERA5_(Year).nc
ids <-list.files(ERA5.files.path, full.names = T)[2:25] %>% 
  map(function(file.paths){
    
    Year<-((basename(file.paths) %>%
      strsplit("\\.") %>%
      unlist)[1] %>%
      strsplit("_")%>%
      unlist)[2]
    
    #adding a record to the input table.
    added<-PEcAn.DB::dbfile.input.insert(
      in.path=file.paths,
      in.prefix='ERA5',
      siteid='1000026755', # This site USA
      startdate=as.Date(paste0(Year,"-01-01")),
      enddate=as.Date(paste0(Year,"-12-31")),
      mimetype="application/x-netcdf",
      formatname="CF Meteorology",
      parentid = NA,
      con,
      hostname = PEcAn.remote::fqdn(),
      allow.conflicting.dates = FALSE
    )
    
    # adding paths to the db files.
    added
  })
