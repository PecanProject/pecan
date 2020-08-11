# script to read in future climate ensemble means from climateNA, which we will use to assess vulnerability to changes in climate:
# climate NA projections downloaded from https://adaptwest.databasin.org/pages/adaptwest-climatena
# specifically, we are using the 48 monthly climate variable projections for 4.5, and 8.5, for the three climate periods:
# overview:
# 1. Read in the lat long data we need to extract climate data over
# 2. read in ClimateNA data & generate fallspr and water year ppt averages for 
# 3. rasterize climate NA data 
# 4 
library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(lubridate)
library(tidync)
pipo.cores.ll <- readRDS("jags.new.data.basic.rds")
cov.data.ll <- pipo.cores.ll$cov.data
#cov.data.ll$LAT <- cov.data.ll$LAT *-1
coordinates(cov.data.ll) <- ~  LON + LAT
proj4string(cov.data.ll) <- CRS("+init=epsg:4326")

cov.data.en <- spTransform(cov.data.ll, CRSobj = CRS("+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 + datum=WGS84 +units=m +no_defs"))
#cov.data.en <- spTransform(cov.data.ll, CRSobj =  crs(tmax.rast))

cov.data.en.df <- data.frame(cov.data.en)
plot(cov.data.en)
plot(cov.data.ll)



hydro.files<- list.files("hydro5/", pattern = ".nc")



hydro.ncs <- paste0(getwd(),"/hydro5/", hydro.files)


x <- hydro.ncs[1]


  # open the netcdf
  nc <- nc_open(x)
  variableofinterest <- names(nc$var)
  # r.nc <- raster(Tmax)
  ppt <- ncvar_get(nc,variableofinterest)
  # 4 Dimensional array:
  # dim 1: long
  # dim 2: lat
  # dim 3: time in months (jan 2018 - Dec 2099)
  # dim 4: each climate model + ensemble member
  lat <- ncvar_get(nc,"latitude")
  lon <- ncvar_get(nc, "longitude")
  projection <- ncvar_get(nc, "projection") # cant get the dimvar
  
  dim(easting)
  dim(northing)
  dim(Tmax)
  
  nTmax <- dim(Tmax)
  nc_close(nc)
  Tmax[,,,1]
  rlist <- list()
  
  
extract.yearly.tmax.fallspr  <- function(proj){ 
  
        # make a raster for each month
      for(i in 1:984){
        rlist[[i]] <- raster(as.matrix(ppt[,,i,proj]), xmn = min(lon), xmx = max(lon), 
                             ymn = min(lat) , ymx = max(lat), 
                             crs = CRS('+init=epsg:4269'))
      }
        
        rast.stack <- stack(rlist)
        plot(rast.stack[[9]])
        plot(cov.data.ll, add = TRUE)
        
        #tmax.rast.ll <- projectRaster(tmax.rast, crs =CRS("+init=epsg:4326") )
       # extracted.pts <- list()
         extracted.pts <- data.frame(raster::extract(rast.stack, cov.data.ll))
         
         extracted.pts$lat <-pipo.cores.ll$cov.data$LAT
         extracted.pts$lon <-pipo.cores.ll$cov.data$LON
         colnames(extracted.pts)[1:984] <- paste0("ppt_", 2018:2099, "_", rep(1:12, 82) )
         extracted.pts.m <- melt(extracted.pts, id.vars = c("lat", "lon"))
         ext.sep <- extracted.pts.m %>% tidyr::separate(variable, sep = "_", into = c("climate", "year", "month"))
          yearly.ppt <- ext.sep %>% dplyr::group_by(lat, lon, climate, year) %>% dplyr::summarise(year.ppt = sum(value))
        yearly.ppt

  
}

all.future.ppt <- list()

system.time(all.future.ppt <- lapply(1:2, FUN = extract.yearly.ppt)) 
# user  system elapsed 
# 11.062   0.749  12.938 

# for list with 2 :
#   user  system elapsed 
# 10.004   0.422  10.508 
system.time(
  for(i in 1:97){
    all.future.ppt[[i]] <- extract.yearly.ppt(i)
  }
  )

x <- hydro.ncs[2]
# open the netcdf
nc <- nc_open(x)

variableofinterest <- names(nc$var)
# r.nc <- raster(Tmax)
Tmax <- ncvar_get(nc,variableofinterest)
# 4 Dimensional array:
# dim 1: long
# dim 2: lat
# dim 3: time in months (jan 2018 - Dec 2099)
# dim 4: each climate model + ensemble member
lat <- ncvar_get(nc,"latitude")
lon <- ncvar_get(nc, "longitude")
projection <- ncvar_get(nc, "projection") # cant get the dimvar

dim(easting)
dim(northing)
dim(Tmax)

nTmax <- dim(Tmax)
nc_close(nc)
Tmax[,,,1]
rlist <- list()

extract.yearly.tmax  <- function(proj){ 
  
  # make a raster for each month
  for(i in 1:984){
    rlist[[i]] <- raster(as.matrix(Tmax[,,i,proj]), xmn = min(lon), xmx = max(lon), 
                         ymn = min(lat) , ymx = max(lat), 
                         crs = CRS('+init=epsg:4269'))
  }
  
  #rast.stack <- stack(rlist)
  #plot(rast.stack[[9]])
  #plot(cov.data.ll, add = TRUE)
  
  #tmax.rast.ll <- projectRaster(tmax.rast, crs =CRS("+init=epsg:4326") )
  # extracted.pts <- list()
  extracted.pts <- data.frame(raster::extract(rast.stack, cov.data.ll))

  extracted.pts$lat <-pipo.cores.ll$cov.data$LAT
  extracted.pts$lon <-pipo.cores.ll$cov.data$LON
  colnames(extracted.pts)[1:984] <- paste0("tmax_", 2018:2099, "_", rep(1:12, 82) )
  extracted.pts.m <- melt(extracted.pts, id.vars = c("lat", "lon"))
  extracted.pts.m$value <- ifelse(extracted.pts.m$value >= 1e+20, NA, extracted.pts.m$value)
  ext.sep <- extracted.pts.m %>% tidyr::separate(variable, sep = "_", into = c("climate", "year", "month"))
  yearly.tmax <- ext.sep %>% dplyr::group_by(lat, lon, climate, year) %>% dplyr::filter(month %in% c("5", "6", "7", "8", "9", "10")) %>% 
    dplyr::summarise(tmax.fall.spr = mean((value), na.rm = TRUE)) # some of the models project tmemparatures will be ~200 deg Farenheight ???
  yearly.tmax
  
  
}

# open all the ncs 

all.future.tmax <- list()


system.time(
  for(i in 1:97){
    all.future.tmax[[i]] <- extract.yearly.tmax(i)
  }
)

# convert to df
all.tmax.df <- do.call(rbind, all.future.tmax)
all.ppt.df <- do.call(rbind, all.future.ppt)

# because the projection labels were not working for this, I need to read in a text file with all the projection names:
proj <- read.delim("hydro5/Projections5.txt", header = FALSE)

#add the projection names to the tmax and ppt data frames
all.tmax.df$proj <- rep(proj$V1, sapply(all.future.tmax , nrow))
all.ppt.df$proj <- rep(proj$V1, sapply(all.future.ppt , nrow))

ppt.models <- all.ppt.df %>% tidyr::separate(proj, sep = -5, into = c("modelrun", "rcp")) #%>% 
  #tidyr::separate(modelrun, sep = "-", into = c("model", "run"))
tmax.models <- all.tmax.df %>% tidyr::separate(proj, sep = -5, into = c("modelrun", "rcp")) 


# okay lets merge these together:
future.climate.ts <- merge(ppt.models, tmax.models, by = c("lat", "lon", "year", "modelrun","rcp"))
hist(future.climate.ts$year.ppt)
future.climate.ts[future.climate.ts$tmax.fall.spr >= 50,]

future.climate.ts <- future.climate.ts %>% dplyr::select(-climate.x, -climate.y)
pipo.cores.ll$cov.data
pipo.cores.ll$future.climate.ts <- future.climate.ts

saveRDS(pipo.cores.ll, "pipo.cores.with.downscaled.hydro.climate.rds")


