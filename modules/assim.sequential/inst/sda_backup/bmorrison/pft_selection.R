library(raster)
library(shapefiles)
library(PEcAn.DB)

analysis = readRDS("/Volumes/data2/bmorrison/sda/500_site_run/output_folder/ANALYSIS.RDS")

dates = names(analysis)
sites = unique(attributes(analysis[[names(analysis)[1]]])$Site)
observations = sites


#working = print(paste("working on: ", i))
sites = observations
bety <- list(user='bety', password='bety', host='modex.bnl.gov',
             dbname='betydb', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con
site_ID <- sites
suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon,
                                            ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})",
                                            ids = site_ID, .con = con))
suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
site_info <- list(site_id=qry_results$id, site_name=qry_results$sitename, lat=qry_results$lat,
                  lon=qry_results$lon, time_zone=qry_results$time_zone)

# load('/Volumes/data/bmorrison/sda/500_site_run/all_lai_data_500.Rdata')
# site_ids = unique(lai_data$site_id)
# sites = data.frame()
# for (i in 1:length(site_ids))
# {
#   index = which(lai_data$site_id == site_ids[i])
#   sites = rbind(sites, lai_data[index[1],])
# }
# sites = sites[,c(5,6,7)]
sites = as.data.frame(cbind(site_info$site_id,site_info$lon, site_info$lat))
names(sites) = c("id", "lon", "lat")
coordinates(sites) = ~lon+lat
projection(sites) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
cover = raster('/Volumes/data/bmorrison/sda/500_site_run/NLCD_2001_Land_Cover_L48_20190424.img')

# sites = shapefile('/data/bmorrison/sda/500_site_run/shapefiles/500_site_selection.shp')
# s = shapefile('/data/bmorrison/sda/500_site_run/shapefiles/500_site_selection.shp')

#sites = as.data.frame(sites)
# sites = sites[, 11:12]
# names(sites) = c("x", "y")
# coordinates(sites) = ~x+y
# projection(sites) = crs(s)

sites = spTransform(sites, CRS = crs(cover))

# make sure projections match
data = extract(cover, sites)
sites$cover = data

# bad = which(data == 11 | data == 12 | data == 31)
# site_data = sites[-bad,]
site_data = sites

ecoregion = shapefile('/Volumes/data2/bmorrison/sda/bailey_paper/data/ecoregions_shapefile/eco_aea_l1.shp')
ecoregion = spTransform(ecoregion, CRS = crs(cover))
eco_data = extract(ecoregion, site_data)
site_data$region = eco_data$NA_L1CODE
site_data$name = eco_data$NA_L1NAME

site_data = as.data.frame(site_data)
names(site_data) = c("ID", "cover", "ecoregion", "name", "lon", "lat")
site_data$pft = NA
site_data$cover = as.numeric(site_data$cover)
site_data$ecoregion = as.numeric(site_data$ecoregion)
# remove sites that are categorized as unclassified, water, ice/snow, barren
index = which(site_data$cover == 0 | site_data$cover == 11 | site_data$cover == 12 | site_data$cover == 31)
site_data$pft[index] = NA

# classify deciduous
index = which(site_data$cover == 41)
site_data$pft[index] = "deciduous"


# classify evergreen/conifer
index = which(site_data$cover == 42)
site_data$pft[index] = "conifer"


# classify mixed forest
index = which(site_data$cover == 43)
site_data$pft[index] = "mixed forest"

# classify developed
index = which(site_data$cover == 21 | site_data$cover == 22 | site_data$cover == 23 | site_data$cover == 24)
site_data$pft[index] = "developed"

# classify shrub/scrub
index = which(site_data$cover == 52 & (site_data$ecoregion == 10 | site_data$ecoregion == 11 | site_data$ecoregion == 12 | site_data$ecoregion == 13 | site_data$ecoregion == 14))
site_data$pft[index] = "arid grassland"

index = which(site_data$cover == 52 & (site_data$ecoregion == 9 | site_data$ecoregion == 8 | site_data$ecoregion == 6 | site_data$ecoregion == 7))
site_data$pft[index] = "mesic grassland"


# classify herbaceous
index = which(site_data$cover == 71 & (site_data$ecoregion == 10 | site_data$ecoregion == 11 | site_data$ecoregion == 12 | site_data$ecoregion == 13 | site_data$ecoregion == 14))
site_data$pft[index] = "arid grassland"

index = which(site_data$cover == 71 & (site_data$ecoregion == 9 | site_data$ecoregion == 15 | site_data$ecoregion == 7 | site_data$ecoregion == 8 | site_data$ecoregion == 5 | site_data$ecoregion == 6))
site_data$pft[index] = "mesic grassland"


# classify hay/pasture crops
index = which((site_data$cover == 81 | site_data$cover == 82) & (site_data$ecoregion == 10 | site_data$ecoregion == 11 | site_data$ecoregion == 12 | site_data$ecoregion == 13 | site_data$ecoregion == 14))
site_data$pft[index] = "arid grassland"

index = which((site_data$cover == 81 | site_data$cover == 82) & (site_data$ecoregion == 9 | site_data$ecoregion == 8 | site_data$ecoregion == 7))
site_data$pft[index] = "mesic grassland"


# classify wetlands
index = which(site_data$cover == 95)
site_data$pft[index] = "mesic grassland"

index = which(site_data$cover == 90)
site_data$pft[index] = "woody wetland"


# LAI analysis for forests (mixed + woody wetland)
index = which(site_data$cover == 43 | site_data$cover == 90)
data = site_data[index,]
coordinates(data) = ~lon+lat
projection(data) = crs(sites)
data = spTransform(data, CRS = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
data = as.data.frame(data, stringsAsFactors = F)


library(PEcAn.data.remote)
site_id = data$ID
site_name = rep(NA, nrow(data))
lat = data$lat
lon = data$lon
time_zone = rep("time", nrow(data))
site_info = list(site_id, site_name, lat, lon, time_zone)
names(site_info) = c("site_id", "site_name", "lat", "lon", "time_zone")

lai = call_MODIS(outdir = NULL, var = "lai", site_info = site_info, product_dates = c("2001001", "2001365"), run_parallel = T, ncores = 10, product ="MOD15A2H", band = "Lai_500m", package_method = "MODISTools", QC_filter = T, progress = F)
ndvi = call_MODIS(outdir = NULL, var = "NDVI", site_info = site_info, product_dates = c("2001001", "2001365"), run_parallel = T, ncores = 10, product ="MOD13Q1", band = "250m_16_days_NDVI", package_method = "MODISTools", QC_filter = F, progress = F)

library(lubridate)

par(mfrow = c(4,5))
info = data.frame()
data = lai
sites = sort(unique(lai$site_id))
# xy = data.frame()
# for (i in 1:length(sites))
# {
#   d = data[which(data$site_id == sites[i]),]
#  xy = rbind(xy, d[1,c(5,7,6)])
# }
#data$calendar_date = as.Date(data$calendar_date)

for (i in 21:40)
{
  site = sites[i]
  d = data[which(data$site_id == site),]
  d = d[,c(2,5,6,7,9)]
  d = d[order(d$calendar_date),]
  d$calendar_date = as.Date(d$calendar_date)
  min = min(d$data, na.rm = T)
  max = max(d$data, na.rm = T)
  difference = max-min
  # winter = d %>%
  #   select(calendar_date, site_id, lat, lon, data) %>%
  #   filter((calendar_date >= month(ymd("2001-01-01")) & calendar_date <= month(ymd("2001-02-28"))) | (calendar_date >= month(ymd("2001-12-01")) & calendar_date <= month(ymd("2001-12-31"))))
  # min = mean(winter$data, na.rm = T)
  
  # summer = d %>%
  #   select(calendar_date, site_id, lat, lon, data) %>%
  #   filter(calendar_date >= month(ymd("2001-06-01")) & calendar_date <= month(ymd("2001-08-30")))
  # max = mean(summer$data, na.rm = T)
  # difference = max - min
  
  info = rbind(info, as.data.frame(cbind(site, d$lon[1], d$lat[1], min, max, difference)))
  plot(d$calendar_date, d$data, ylim = c(0, max(data$data)+2), main = site)
}






