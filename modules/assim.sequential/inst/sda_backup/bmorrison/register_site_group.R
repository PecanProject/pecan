library(raster)
library(shapefiles)
------------- Load required libraries ---------------------------------------------------------#
library(PEcAn.all)
library(PEcAn.SIPNET)
library(PEcAn.LINKAGES)
library(PEcAn.visualization)
library(PEcAn.assim.sequential)
library(nimble)
library(lubridate)
library(PEcAn.visualization)
#PEcAn.assim.sequential::
library(rgdal) # need to put in assim.sequential
library(ncdf4) # need to put in assim.sequential
library(purrr)
library(listviewer)
library(dplyr)
library(furrr)
library(tictoc)

data = shapefile('/data/bmorrison/sda/500_site_run/shapefiles/500_site_selection_final.shp')
data = as.data.frame(data)
names(data) = c("type", "lon", "lat")


bety <- list(user='bety', password='bety', host='localhost',
             dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con
#site_ID <- observation

#-- register sites
site_id <-map2(data$lon, data$lat, function(lon, lat){
  pr<-paste0(round(lon,0), round(lat,0))
  out <-db.query(paste0("INSERT INTO sites (sitename, geometry) VALUES ('CMS_500_SDA_",pr,"', ",
                        "ST_SetSRID(ST_MakePoint(",lon,",", lat,", 1000), 4326) ) RETURNING id, sitename"),
                 con
  )
  out
})
#link to site group
site_id %>%
  map(~ db.query(paste0("INSERT INTO sitegroups_sites (sitegroup_id , site_id ) VALUES (2000000009, ",
                        .x[[1]],")"),con))