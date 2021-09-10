#download ERA5 met data
library(dplyr)
library(reticulate)
library(future)
library(purrr)
library(furrr)
library(PEcAn.all)
library(PEcAn.data.atmosphere)
library(xts)
plan(multiprocess)

cal.ERA5.area <- function(lon, lat){
  lon.floor <- floor(lon)
  lon.ceil <- ceiling(lon)
  
  lat.floor <- floor(lat)
  lat.ceil <- ceiling(lat)
  
  top.left.lon <- lon.floor - 2
  top.left.lat <- lat.ceil + 2
  
  bottom.right.lon <- lon.ceil + 2
  bottom.right.lat <- lat.floor - 2
  
  return(paste0(as.character(top.left.lat), "/", as.character(top.left.lon),
                "/", as.character(bottom.right.lat), "/", as.character(bottom.right.lon)))
}
#db.query
dbparms = list()
dbparms$dbname = "bety"
dbparms$host = "128.197.168.114"
dbparms$user = "bety"
dbparms$password = "bety"

#Connection code copied and pasted from met.process
bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                            host     = dbparms$host, 
                            user     = dbparms$user, 
                            password = dbparms$password)
con <- bety$con

settings <-PEcAn.settings::read.settings("/projectnb/dietzelab/dongchen/Multi-site/pecan.SDA.4sites.xml")
site_1 <- settings[[1]]
met.out.path <- "/projectnb/dietzelab/dongchen/Multi-site/ERA5"
site.id <- site_1$run$site$id
lonlat <- db.site.lat.lon(site.id, con)
area <- cal.ERA5.area(lonlat$lon, lonlat$lat)
start_year <- 2008
end_year <- 2010
setwd(met.out.path)
c(start_year:end_year) %>%
  future_map(function(year) {
    cdsapi <-import("cdsapi")
    c <- cdsapi$Client()
    
    c$retrieve(
      'reanalysis-era5-single-levels',
      list(
        'product_type' = 'ensemble_members',
        'format' = 'netcdf',
        'day' = list('01','02','03',
                     '04','05','06',
                     '07','08','09',
                     '10','11','12',
                     '13','14','15',
                     '16','17','18',
                     '19','20','21',
                     '22','23','24',
                     '25','26','27',
                     '28','29','30',
                     '31'),
        'time' = list('00:00','03:00','06:00',
                      '09:00','12:00','15:00',
                      '18:00','21:00'),
        'month' = list('01','02','03',
                       '04','05','06',
                       '07','08','09',
                       '10','11','12'),
        'year' = as.character(year),
        #'49.6/-125.11/25.1/-67.1'
        #
        'area' = area,
        'variable' = list( "2m_temperature","surface_pressure",
                           "2m_dewpoint_temperature","total_precipitation",                
                           "10m_u_component_of_wind","10m_v_component_of_wind",
                           "surface_solar_radiation_downwards","surface_thermal_radiation_downwards")
      ),
      paste0('ERA5_',year,'.nc')
    )
    #file.copy(paste0("~/",'ERA5_',year,'.nc'), "/projectnb/dietzelab/dongchen/Multi-site/ERA5")
  })


#Register ERA5
#db.query
dbparms = list()
dbparms$dbname = "bety"
dbparms$host = "128.197.168.114"
dbparms$user = "bety"
dbparms$password = "bety"

#Connection code copied and pasted from met.process
bety <- dplyr::src_postgres(dbname   = dbparms$dbname, 
                            host     = dbparms$host, 
                            user     = dbparms$user, 
                            password = dbparms$password)
con <- bety$con
#ERA5.extracted.path <- "/projectnb/dietzelab/dongchen/Multi-site/ERA5/output"
#adding a record to the input table.
added<-PEcAn.DB::dbfile.input.insert(
  in.path=met.out.path,
  in.prefix='ERA5_',
  siteid=site_1$run$site$id, # This is site USA
  startdate=as.Date(paste0(2008,"-01-01"), tz="UTC"), # look into the files and make sure you have all the files for this time period
  enddate=as.Date(paste0(2010,"-12-31"), tz="UTC"),
  mimetype="application/x-netcdf",
  formatname="CF Meteorology",
  parentid = NA,
  con,
  hostname = PEcAn.remote::fqdn(),
  allow.conflicting.dates = T
)

#db.file check
db.file <- PEcAn.DB::dbfile.input.check(
  siteid="1000025731",
  startdate = "2008-01-01",
  enddate = "2010-12-31",
  parentid = NA,
  hostname = PEcAn.remote::fqdn(),
  mimetype="application/x-netcdf",
  formatname="CF Meteorology",
  exact.dates = T,
  con = con,
  pattern = "ERA5",
  return.all=TRUE
) 
#Extract ERA5
library(xts)
settings <-PEcAn.settings::read.settings("/projectnb/dietzelab/dongchen/Multi-site/4site.test.xml")
site_1 <- settings[[1]]
lat <- as.numeric(site_1$run$site$lat)
lon <- as.numeric(site_1$run$site$lon)
inp.path <- "/projectnb/dietzelab/dongchen/Multi-site/ERA5/"
out.path <- "/projectnb/dietzelab/dongchen/Multi-site/ERA5/output"
start.date <- "2010/01/01"
end.date <- "2020/12/31"
inp.prefix <- "ERA5_"
newsite <- site_1$run$site$name
ERA5_ex <- extract.nc.ERA5(slat = lat, slon = lon, in.path = inp.path, start_date = start.date, end_date = end.date, outfolder = out.path, in.prefix = inp.prefix, newsite = newsite)


#go through met.process
library(PEcAn.all)
library(PEcAn.data.atmosphere)
settings <-PEcAn.settings::read.settings("/projectnb/dietzelab/dongchen/Multi-site/4site.test.xml")
site_1 <- settings[[1]]
site <- site_1$run$site 
input_met <- list(source="ERA5", output="SIPNET")#site_1$run$inputs$met
start_date <- as.Date(paste0(2010,"-01-01"), tz="UTC")
end_date <- as.Date(paste0(2020,"-12-31"), tz="UTC")
model <- site_1$model$type
host <- "localhost"
dbparms <- site_1$database$bety
dir <- "/projectnb/dietzelab/dongchen/Multi-site/ERA5/output1"
browndog = NULL
spin=NULL
overwrite = FALSE


#delete a record
library(DBI)
#for multiple files: to_vec(for(id in ids) ids)
delete_queries = paste0("DELETE FROM dbfiles WHERE (id = '", 1000758008         , "');")
dbExecute(con, delete_queries)
delete_queries = paste0("DELETE FROM inputs WHERE (id = '", 1000469350        , "');")
dbExecute(con, delete_queries)


#using function to download ERA5 data
met.out <- "/projectnb/dietzelab/dongchen/Multi-site/ERA5/new_ERA5"
site.id <- site_1$run$site$id
lonlat <- db.site.lat.lon(site.id, con)
area <- rep(round(c(lonlat$lat, lonlat$lon) * 4) / 4, 2)

files <- download.ERA5.old(
          met.out,
          start_date = "2007-01-01",
          end_date = "2010-12-31",
          lat.in = lonlat$lat,
          lon.in = lonlat$lon,
          product_types = "all"
         )
