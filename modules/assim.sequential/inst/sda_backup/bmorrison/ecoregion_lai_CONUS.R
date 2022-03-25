library(raster)
library(shapefiles)
library(sp)
library(PEcAn.data.remote)
library(sf)
library(dplyr)
library(rgdal)
library(tidyr)
library(rgeos)
library(rgbif)
library(viridis)
library(gridExtra)
library(rasterVis)
library(doParallel)
library(PEcAn.utils)
set.seed(1)

#eco = st_read(dsn = '/data/bmorrison/sda/ecoregion_site_analysis/shapefiles', layer  = 'eco_conus_rename')
setwd('/data/bmorrison/sda/ecoregion_site_analysis/modis_data/CONUS')
states = st_read(dsn = "/data/bmorrison/sda/ecoregion_site_analysis/shapefiles/states_21basic/states.shp")
states = as(states, "Spatial")

### testing on 1 ecoregion
region = states
region = st_read(dsn = "/data/bmorrison/sda/ecoregion_site_analysis/shapefiles/states_21basic/states.shp")
region = region[-c(1,28,51),]
#region = eco[eco$name == eco$name[11],]
region = st_union(region)
region = as(region, "Spatial")
region = spTransform(region, CRS = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")
region_ll = spTransform(region, CRS = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

# hexagonal tesellation random sampling

# must be in meters 
make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}

# trick to figure out how many polygons I want vs. cell area of hexagons
n <- 1000

area_of_region = raster::area(region)
cell_area= area_of_region/n

# make hexagonal tesselation grid
hex_grid <- make_grid(region, cell_area = cell_area, clip = FALSE)
#hex_grid <- make_grid(region, cell_diameter = 37894.1, clip = FALSE)

plot(region, col = "grey50", bg = "light blue")
plot(hex_grid, border = "orange", add = T)
# clip to ecogreion area

save(hex_grid, file = paste("/data/bmorrison/sda/ecoregion_site_analysis/hex_grid_CONUS.Rdata", sep = ""))
load(paste("/data/bmorrison/sda/ecoregion_site_analysis/hex_grid_CONUS.Rdata", sep = ""))

# randomly select one point from each hexagon (random)
samples = data.frame()
for (i in 1:length(names(hex_grid)))
{
  hex = hex_grid[i,]
  sample = as.data.frame(spsample(hex, n = 1, type = 'random'))
  names(sample) = c("x", "y")
  samples = rbind(samples, sample)
}
coordinates(samples) = ~x+y
projection(samples) = crs(region)

# clip out points outside of ecoregion area
samples <- gIntersection(samples, region, byid = TRUE)

plot(region, col = "grey50", bg = "light blue", axes = TRUE)
plot(hex_grid, border = "orange", add = T)
plot(samples, pch = 20, add = T)
samples = spTransform(samples, CRS = crs(states))
region = spTransform(region, CRS = crs(states))


xy = as.data.frame(samples)
names(xy) = c("lon", "lat")
save(xy, file = paste('/data/bmorrison/sda/ecoregion_site_analysis/random_sites_CONUS.Rdata', sep = ""))
# extract MODIS data for location

load("/data/bmorrison/sda/ecoregion_site_analysis/random_sites_CONUS.Rdata")


product = "MOD15A2H"

dates = PEcAn.utils::retry.func(MODISTools::mt_dates(product, lat = xy$lat[1], lon = xy$lon[1]), maxError = 10, sleep = 2)

starting_dates = dates$calendar_date[grep(dates$calendar_date, pattern = "2001-01")]
start_count = as.data.frame(table(starting_dates), stringsAsFactors = F)
start_date = gsub("-", "/", start_count$starting_dates[1])

ending_dates = dates$calendar_date[grep(dates$calendar_date, pattern = "2018-12")]
end_count = as.data.frame(table(ending_dates), stringsAsFactors = F)
end_date = gsub("-", "/", end_count$ending_dates[nrow(end_count)] )

# 10 cpu limit because THREADDS has 10 download limit
#xy = xy[1:nrow(xy),]

cl <- parallel::makeCluster(10) #, outfile= "")
doParallel::registerDoParallel(cl)

output = data.frame()
for (j in 1:ceiling(nrow(xy)/10))
{
  if (j == ceiling(nrow(xy)/10))
  {
    coords = xy[((j*10)-9):nrow(xy),] 
    working = print(paste("working on : ", ((j*10)-9), "-", nrow(xy), sep = ""))
    
  } else {
    coords = xy[((j*10)-9):(j*10),]
    working = print(paste("working on : ", ((j*10)-9), "-", (j*10), sep = ""))
    
  }
  #siteID = paste(round(coords[i,], digits = 2), collapse = "_")
  start = Sys.time()
  data = PEcAn.utils::retry.func(foreach(i=1:nrow(coords), .combine = rbind) %dopar% PEcAn.data.remote::call_MODIS(outfolder = getwd(), iter = ((j*10-10)+i), product = "MOD15A2H",band = "Lai_500m", start_date = start_date, end_date = end_date, lat = coords$lat[i], lon = coords$lon[i],size = 0, band_qc = "FparLai_QC", band_sd = "", package_method = "MODISTools", QC_filter = T), maxError = 10, sleep = 2)
  end = Sys.time()
  difference = end-start
  time = print(difference)
  output = rbind(output, data)
}
stopCluster(cl)


save(output, file = paste('/data/bmorrison/sda/ecoregion_site_analysis/modis_data_output_', nrow(xy), '.Rdata', sep = ""))
# 
# load(paste('/data/bmorrison/sda/ecoregion_site_analysis/modis_data_output_', nrow(output), '.Rdata', sep = ""))
# output = as.data.frame(output, row.names = NULL)

# for large datasets to group together
files = list.files(path = '/data/bmorrison/sda/ecoregion_site_analysis/modis_data/CONUS', pattern = '.csv', include.dirs = T, full.names = T)
xy = data.frame()
for (i in 1:length(files))
{
  f = read.csv(files[i])
  xy = rbind(xy, f)
}

output = xy
# summarize into anual peak lai from 2001-2018
years = lubridate::year(start_date):lubridate::year(end_date)

data = output
sites = output
coordinates(sites) = ~lon+lat
projection(sites) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
sites = as.data.frame(unique(coordinates(sites)))
coordinates(sites) = ~lon+lat
projection(sites) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "


compute_annual_lai = function(data, sites)
{
  index = which((round(data$lon, digits = 3)== round(sites$lon, digits = 3)) & (round(data$lat, digits = 3) == round(sites$lat, digits = 3)))
  if (length(index) > 0)
  {
    site = data[index,]
    years = unique(lubridate::year(site$calendar_date))
    
    summary = data.frame()
    for (j in 1:length(years))
    {
      g = grep(site$calendar_date, pattern = years[j])
      if (length(g) > 0)
      {
        d = site[g,]
        percentile = which(d$data <= quantile(d$data, probs = 0.95, na.rm = T)[1])
        peak = max(d$data[percentile], na.rm = T)
        
        info = d[1,]
        info$data = peak
        info$calendar_date = years[j]
        
        summary = rbind(summary, info)
      }
    }
    peak_lai = summary[1,]
    peak_lai$data = max(summary$data[which(summary$data <= quantile(summary$data, probs = 0.95))], na.rm = T)
    return(peak_lai)
  }
}

cl <- parallel::makeCluster(10) #, outfile= "")
doParallel::registerDoParallel(cl)

test = foreach(i=1:nrow(sites), .combine = rbind) %dopar% compute_annual_lai(data = data, sites = sites[i,])

stopCluster(cl)

test = data.frame()
for (i in 1:nrow(coordinates(sites)))
{
  t = compute_annual_lai(data = data, sites = sites[i,])
  test = rbind(test, t)
}


# 
# 
# 
# summary =data.frame()
# for (i in 1:nrow(xy))
# {
#   index = which(round(output$lon, digits =3) == round(xy$lon[i], digits = 3) & round(output$lat, digits = 3) == round(xy$lat[i], digits = 3))
#   if (length(index)>0)
#   {
#     site = output[index,]
#     for (j in 1:length(years))
#     {
#       g = grep(site$calendar_date, pattern = years[j])
#       if (length(g) > 0)
#       {
#         d = site[g,]
#         percentile = which(d$data <= quantile(d$data, probs = 0.95, na.rm = T)[1])
#         peak = max(d$data[percentile], na.rm = T)
#         
#         info = d[1,]
#         info$data = peak
#         info$calendar_date = years[j]
#         
#         summary = rbind(summary, info)
#       }
#     }
#   }
# }
# 
# peak_lai = data.frame()
# for (i in 1:nrow(xy))
# {
#   index = which(round(summary$lat, digits = 3) == round(xy$lat[i], digits = 3) & round(summary$lon, digits = 3) == round(xy$lon[i], digits = 3))
#   
#   if (length(index) >0)
#   {
#     site = summary[index,]
#     
#     peak = mean(site$data, na.rm = T)
#     info = site[1,]
#     info$data = peak
#     peak_lai = rbind(peak_lai, info)
#   }
# }
# 
# peak_lai = as.data.frame(peak_lai, row.names = NULL)
# semivariogram analysis

#1. reproject spatial data into aea so distances are in meteres
coordinates(test) = ~lon+lat
projection(test) = crs(sites)
test = spTransform(test, CRS = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

library(gstat)  
# 1. check that data is normally distributed, if not, transform.
hist(test$data)

library(MASS)
norm = fitdistr(x = test$data, densfun = "normal")
test$trans= rnorm(test$data, mean = norm$estimate[1], sd = norm$estimate[2])

v = variogram(trans~1, data = test)
v.data = v[order(v$dist),]
plot(v)

v.vgm = vgm( psill = NA, range = NA, model = "Sph", nugget = 1)
v.fit = fit.variogram(v, v.vgm, fit.sills = T, fit.ranges = T, fit.kappa = T)
plot(v, model = v.fit)



cell_area= 37894.1

# make hexagonal tesselation grid
hex_grid <- make_grid(region, cell_area = cell_area, clip = FALSE)

plot(region, col = "grey50", bg = "light blue")
plot(hex_grid, border = "orange", add = T)
# clip to ecogreion area

samples = data.frame()
for (i in 1:length(names(hex_grid)))
{
  hex = hex_grid[i,]
  sample = as.data.frame(spsample(hex, n = 1, type = 'random'))
  names(sample) = c("x", "y")
  samples = rbind(samples, sample)
}
coordinates(samples) = ~x+y
projection(samples) = crs(region)

# clip out points outside of ecoregion area
samples <- gIntersection(samples, region, byid = TRUE)

