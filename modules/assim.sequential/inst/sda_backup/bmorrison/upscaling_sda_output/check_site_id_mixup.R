library(PEcAn.DB)
library(PEcAn.settings)
library(shapefiles)
library(raster)
############################ SDA run information
analysis = readRDS('/Volumes/data2/bmorrison/sda/500_site_run/output_folder/ANALYSIS.RDS')
dates = names(analysis)
sites = unique(attributes(analysis[[names(analysis)[1]]])$Site)
sda_obs = sites


site_ID <- sda_obs
suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                            ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                            ids = site_ID, .con = con))

suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
sda_site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                       lon=qry_results$lon, time_zone=qry_results$time_zone)

############################# BETY information
settings <- read.settings("/Volumes/data/bmorrison/sda/500_site_run/pecan_MultiSite_SDA_LAI_AGB_sitegroup_500.xml")

bety <- list(user='bety', password='bety', host='modex.bnl.gov',
             dbname='betydb', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con

if ("sitegroup" %in% names(settings)){
  if (is.null(settings$sitegroup$nSite)){
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings,
                                                             sitegroupId = settings$sitegroup$id, con = con)
  } else {
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings,
                                                             sitegroupId = settings$sitegroup$id,
                                                             nSite = settings$sitegroup$nSite)
  }
  settings$sitegroup <- NULL ## zero out so don't expand a second time if re-reading
}

observation <- c()
for (i in seq_along(1:length(settings$run))) {
  command <- paste0("settings$run$settings.",i,"$site$id")
  obs <- eval(parse(text=command))
  observation <- c(observation,obs)
}

bety_sites = observation

site_ID <- bety_sites
suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                            ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                            ids = site_ID, .con = con))

suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
bety_site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                  lon=qry_results$lon, time_zone=qry_results$time_zone)

######################## COMPARE SITE ID INFORMATION ##############################
#bety sites will have the extra sites up to 517 because this group was made before the sites with bad modis data was removed.
bety_sites = bety_site_info$site_id
sda_sites = sda_site_info$site_id

### lets make sure they have the correct number of sites matching by id
length(which(sda_sites %in% bety_sites))
# 347 sites --> this points to the fact that there are different site ids numbers between the 517 site group and the site ids in the output sda output
# SITE IDS DO NOT MATCH THE OUTPUT OF THE SDA #

# Do the coordinates match up at least?
sda_data = as.data.frame(cbind(sda_site_info$site_id, sda_site_info$lon, sda_site_info$lat))
names(sda_data) = c("id", "lon", "lat")
bety_data = as.data.frame(cbind(bety_site_info$site_id, bety_site_info$lon, bety_site_info$lat))
names(bety_data) = c("id", "lon", "lat")
coordinates(sda_data) = ~lon+lat
coordinates(bety_data) = ~lon+lat
projection(sda_data) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
projection(bety_data) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "

us = shapefile('/Volumes/data2/bmorrison/sda/500_site_run/output_folder/us_border_correct.shp')
us = spTransform(us, CRS = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

par(mfrow = c(2,1))
plot(us)
plot(sda_data, add = T, pch = 20, col = 'blue')
plot(us)
plot(bety_data, add = T, pch = 20, col = 'red')

## Coorindates appear to be the same locations. lets make for sure.
sda_data = as.data.frame(cbind(sda_site_info$site_id, sda_site_info$lon, sda_site_info$lat))
names(sda_data) = c("id", "lon", "lat")
bety_data = as.data.frame(cbind(bety_site_info$site_id, bety_site_info$lon, bety_site_info$lat))
names(bety_data) = c("id", "lon", "lat")
count = vector()
for (i in 1:nrow(sda_data))
{
  index = which(bety_data$lon == sda_data$lon[i] & bety_data$lat == sda_data$lat[i])
  if (length(index) > 0)
  {
    count = c(count, i)
  }
}