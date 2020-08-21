# script to readin in the downscaled climate model projections from https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/#Projections:%20Complete%20Archives
# Under Subset request, I selected downscaled projections for Jan 2018 - Dec 2099 and highlighted the region of AZ for the domain
# I used the projection set: BCSD-CMIP5-Hydrology-monthly and selected maximum temperature and precipiation
# then for all the rcps, I selected "all"
# then I selected "no analysis" and "netcdf" on the last page...it took less than an hour for them to email me with a link to download the zipped data
# #######################more information from the product:
# Product:               Bias-corrected statistically downscaled GCM data
# Variables:             tasmax    
# NetCDF type:          float     
# NetCDF missing value:  1e+20     
# Period:                2018Jan through 2099Dec
# Resolution:            1/8 degree
# Latitude bounds:       (29.875, 38.125)
# Longitude bounds:      (-115.125, -108.0)
# Area within bounds:    602058 km^2 (approx)
# Dimensions:         
#   Times:                984
# Latitudes:            66
# Longitudes:           57
# Projections:          97
# 
# 
# Global attributes
# -----------------
#   Conventions:           GDT 1.2
# authors:               Bridget Thrasher, Ed Maurer
# description:           Bias-corrected and downscaled GCM data
# creation_date:         2012
# institution:           Climate Analytics Group, Santa Clara U.
# SurfSgnConvention:     Traditional


# Selected 
# overview:
# 1. Read in the lat long data we need to extract climate data over
# 2. create function to open netcdf, generate a raster stack of all the projections, then extract by our lat long data
# 3. output & repeat for the next climate variable

library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
library(ncdf4) # a must have package for opening netcdfs
library(lubridate)
library(tidync)

#------------------------------------------------------------------
# 1. Read in the lat long data we need to extract climate data over
#------------------------------------------------------------------

# read in the data set that has the lat long of the plots/cores we want to extract projections from
pipo.cores.ll <- readRDS("jags.new.data.basic.rds") # read in my list with covariate data
cov.data.ll <- pipo.cores.ll$cov.data # the covariate data with lat long 

coordinates(cov.data.ll) <- ~  LON + LAT
proj4string(cov.data.ll) <- CRS("+init=epsg:4326")

# dont need this, but this would be the way to transform to a new projection (needed for climat NA)
cov.data.en <- spTransform(cov.data.ll, CRSobj = CRS("+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 + datum=WGS84 +units=m +no_defs"))

cov.data.en.df <- data.frame(cov.data.en)
plot(cov.data.en)
plot(cov.data.ll)


#------------------------------------------------------------------
# 2. create function to open netcdf, generate a raster stack of all the projections, then extract by our lat long data
#------------------------------------------------------------------

# list all the netcdfs
hydro.files <- list.files("hydro5/", pattern = ".nc")

hydro.ncs <- paste0(getwd(),"/hydro5/", hydro.files)

# for the precipitation:
x <- hydro.ncs[1]


  # open the netcdf
  nc <- nc_open(x)
  variableofinterest <- names(nc$var) # get the variable of interest
  ppt <- ncvar_get(nc,variableofinterest) # this extracts a 4 dimensional array of data
  # 4 Dimensional array:
  # dim 1: long
  # dim 2: lat
  # dim 3: time in months (jan 2018 - Dec 2099)
  # dim 4: each climate model/ensemble member
  lat <- ncvar_get(nc,"latitude") # get lat
  lon <- ncvar_get(nc, "longitude") # get long
  nc.time <- ncvar_get(nc, "time") # get long
  projection <- ncvar_get(nc, "projection") # cant get the dimvar, but metadata has info on projections
  
  
  dim(ppt)# look at the dimensions
  nmonths <- dim(ppt)[3] # 3rd dimension is the number of months in the downscaled projections
  nTmax <- dim(ppt)
  nc_close(nc) # close the netcdf file when you are done extracting

  
  
  rlist <- list()
  
  # this function takes a given projection, makes a raster stack where each raster is a month for a given climate model run projection
  # and extracts the monthly time series for each lat long point of interest, then summs across year to get a data frame of
  #  columns: lat   lon climate year  year.ppt
  # inputs: proj = a number of which climate ensemble you want 
  # ppt = the 4 D array 
  # cov.data.ll = the spatial object to extract by
  # nmonths = # months from the 4D array
extract.yearly.ppt  <- function(proj , ppt, cov.data.ll , nmonths ){ 
     rlist <- list()
        # make a raster for each month 
      for(m in 1:nmonths){ 
        rlist[[m]] <- raster(as.matrix(ppt[,,m,proj]), xmn = min(lon), xmx = max(lon), 
                             ymn = min(lat) , ymx = max(lat), 
                             crs = CRS('+init=epsg:4269'))
      }
        
        rast.stack <- stack(rlist)
        #plot(rast.stack[[9]]) # can plot for sanity
        #plot(cov.data.ll, add = TRUE)
        
        #tmax.rast.ll <- projectRaster(tmax.rast, crs =CRS("+init=epsg:4326") ) # dont recommend trying to change projections of the rasters, it will take much much longer to run this
       
         extracted.pts <- data.frame(raster::extract(rast.stack, cov.data.ll))
         ll.data <- as.data.frame(cov.data.ll)
         extracted.pts$lat <-ll.data$LAT # get the lat and long
         extracted.pts$lon <-ll.data$LON
         
         colnames(extracted.pts)[1:nmonths] <- paste0("ppt_", rep(2018:2099, each = 12), "_", rep(1:12, 82) ) # note may need to change this to make more customizable
         extracted.pts.m <- melt(extracted.pts, id.vars = c("lat", "lon"))
         extracted.pts.m$value <- ifelse(extracted.pts.m$value >= 1e+20, NA, extracted.pts.m$value)
         ext.sep <- extracted.pts.m %>% tidyr::separate(variable, sep = "_", into = c("climate", "year", "month"))
         # I use yearly ppt, but we could make a different summary of interest here
          yearly.ppt <- ext.sep %>% dplyr::group_by(lat, lon, climate, year) %>% dplyr::summarise(year.ppt = sum(value)) 
        yearly.ppt

  
}


# then apply this funcation across all 97 projections downloaded in the netcdf
all.future.ppt <- list()

#system.time(all.future.ppt <- lapply(1:2, FUN = extract.yearly.ppt)) 
# user  system elapsed 
# 11.062   0.749  12.938 

# for loop with 2 :
#   user  system elapsed 
# 10.004   0.422  10.508 

# for loop is slightly faster, so we will use that, but this takes a bit
system.time(
  for(i in 1:97){ # extracts for all 97 projections 
    all.future.ppt[[i]] <- extract.yearly.ppt(proj = i, ppt= ppt,  cov.data.ll = cov.data.ll, nmonths = nmonths)
  }
  )



#------------------------------------------------
# for temperature just use the second netcdf in our downloaded package.
x <- hydro.ncs[2]
# open the netcdf
nc <- nc_open(x)

variableofinterest <- names(nc$var)

Tmax <- ncvar_get(nc,variableofinterest)
# 4 Dimensional array:
# dim 1: long
# dim 2: lat
# dim 3: time in months (jan 2018 - Dec 2099)
# dim 4: each climate model + ensemble member
lat <- ncvar_get(nc,"latitude")
lon <- ncvar_get(nc, "longitude")
projection <- ncvar_get(nc, "projection") # cant get the dimvar


dim(Tmax)

nTmax <- dim(Tmax)
nmonths <- dim(Tmax)[3] # 3rd dimension is the number of months in the downscaled projections

nc_close(nc)
Tmax[,,,1]
rlist <- list()


# created a second function because I want to summarise the temp data in a different way
extract.yearly.tmax  <- function(proj,Tmax, cov.data.ll, nmonths ){ 
  
  # make a raster for each month
  for(i in 1:984){
    rlist[[i]] <- raster(as.matrix(Tmax[,,i,proj]), xmn = min(lon), xmx = max(lon), 
                         ymn = min(lat) , ymx = max(lat), 
                         crs = CRS('+init=epsg:4269'))
  }
  
  rast.stack <- stack(rlist)
  #plot(rast.stack[[9]])
  #plot(cov.data.ll, add = TRUE)
  
  #tmax.rast.ll <- projectRaster(tmax.rast, crs =CRS("+init=epsg:4326") )
  # extracted.pts <- list()
  extracted.pts<- data.frame(raster::extract(rast.stack, cov.data.ll))

  ll.data <- as.data.frame(cov.data.ll)
  extracted.pts$lat <-ll.data$LAT # get the lat and long
  extracted.pts$lon <-ll.data$LON
  colnames(extracted.pts)[1:984] <- paste0("tmax_", rep(2018:2099, each = 12), "_", rep(1:12, 82) )
  extracted.pts.m <- melt(extracted.pts, id.vars = c("lat", "lon"))
  extracted.pts.m$value <- ifelse(extracted.pts.m$value >= 1e+20, NA, extracted.pts.m$value)
  ext.sep <- extracted.pts.m %>% tidyr::separate(variable, sep = "_", into = c("climate", "year", "month"))
  
  # get the equivalent of our spring-fall tmax
  yearly.tmax <- ext.sep %>% dplyr::group_by(lat, lon, climate, year) %>% dplyr::filter(month %in% c("5", "6", "7", "8", "9", "10")) %>% 
    dplyr::summarise(tmax.fall.spr = mean((value), na.rm = TRUE)) # some of the models project tmemparatures will be ~200 deg Farenheight ???
  yearly.tmax
  
  
}

# open all the ncs 

all.future.tmax <- list()


system.time(
  for(i in 1:97){
    all.future.tmax[[i]] <- extract.yearly.tmax(i, Tmax = Tmax, cov.data.ll = cov.data.ll, nmonths = nmonths )
  }
)

# convert to df
all.tmax.df <- do.call(rbind, all.future.tmax)
all.ppt.df <- do.call(rbind, all.future.ppt)

# because the projection labels were not working for this, I need to read in a text file with all the projection names:
proj <- read.delim("hydro5/Projections5.txt", header = FALSE)
#proj.tas <- read.delim("bcsd5/Projections5.txt", header = FALSE)

#add the projection names to the tmax and ppt data frames
all.tmax.df$proj <- rep(proj$V1, sapply(all.future.tmax , nrow))
all.ppt.df$proj <- rep(proj$V1, sapply(all.future.ppt , nrow))

ppt.models <- all.ppt.df %>% tidyr::separate(proj, sep = -5, into = c("modelrun", "rcp")) #%>% 
  #tidyr::separate(modelrun, sep = "-", into = c("model", "run"))
tmax.models <- all.tmax.df %>% tidyr::separate(proj, sep = -5, into = c("modelrun", "rcp")) 
summary.tas <- tmax.models %>% dplyr::group_by(lat, lon, year, rcp) %>% dplyr::summarise(sd = sd(tmax.fall.spr, na.rm = TRUE), 
                                                            mean = mean(tmax.fall.spr, na.rm = TRUE))

summary.ppt <- ppt.models %>% dplyr::group_by(lat, lon, year, rcp) %>% dplyr::summarise(sd = sd(ppt, na.rm = TRUE), 
                                                                                         mean = mean(ppt, na.rm = TRUE))

# okay lets merge these together:
future.climate.ts <- merge(ppt.models, tmax.models, by = c("lat", "lon", "year", "modelrun","rcp"))
hist(future.climate.ts$year.ppt)
future.climate.ts[future.climate.ts$tmax.fall.spr >= 50,]

future.climate.ts <- future.climate.ts %>% dplyr::select(-climate.x, -climate.y)
pipo.cores.ll$cov.data
pipo.cores.ll$future.climate.ts <- future.climate.ts

saveRDS(pipo.cores.ll, "pipo.cores.with.downscaled.hydro.ppt.climatev3.rds")




