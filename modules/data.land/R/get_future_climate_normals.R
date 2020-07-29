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



# library(raster)
# r = raster("/Users/kah/Downloads/CCSM4_rcp45_2025_Bioclim_ASCII/CCSM4_rcp45_2025_PPT_wt.asc")
# plot(r)
# plot(cov.data.en, add = TRUE)
# extent(r)


rcp45.2020s.name <- list.files("climateNA_future/NA_ENSEMBLE_rcp45_2020s_Monthly_netCDF/", pattern = ".nc")

full.rcp45.2020s <- paste0(getwd(),"/climateNA_future/NA_ENSEMBLE_rcp45_2020s_Monthly_netCDF/", rcp45.2020s.name)

x <- full.rcp45.2020s[2]



extract.climateNA <- function(x){
      # open the netcdf
      nc <- nc_open(x)
      variableofinterest <- names(nc$var)
    # r.nc <- raster(Tmax)
      Tmax <- ncvar_get(nc,variableofinterest)
      easting <- ncvar_get(nc,"easting")
      #easting <- easting/100000
      northing <- ncvar_get(nc, "northing")
      #easting <- easting/100000
      dim(easting)
      dim(northing)
      dim(Tmax)
      colnames(Tmax) <- northing
      rownames(Tmax) <- easting
      nTmax <- dim(Tmax)
      nc_close(nc)
     
     
      #NOTEEEEE:######********
      # there is apparently a typo in the proj4string listing in the climateNA netcdf, it should be:
      #lon_0=-95.0
      # spend way too long trying to figure out what was wrong with my projections....
      # maybe they should fix this instead of just putting it in a small note outside of the metadata...
      tmax.rast <- raster(t(Tmax), xmn = min(easting), xmx = max(easting), 
                          ymn = min(northing) , ymx = max(northing) , 
                          crs = CRS("+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 + datum=WGS84 +units=m +no_defs"))
      
      
      #tmax.rast.ll <- projectRaster(tmax.rast, crs =CRS("+init=epsg:4326") )
      extracted.pts <- raster::extract(tmax.rast, cov.data.en)
      #extracted.pts <- raster::extract(r, cov.data.ll)
      
      #plot(tmax.rast)
      #plot(tmax.rast.ll)
      #points( cov.data.en, col= "red")
      # reproject to lat long of FIA data
      
      df<- data.frame(#easting = cov.data.en.df$LON, 
                      #northing = cov.data.en.df$LAT,
                      var = extracted.pts)
      colnames(df)[1] <- variableofinterest
      df
      
}


system.time(extracted.ppts <- lapply(full.rcp45.2020s[1:12], FUN = extract.climateNA)) # open all the ncs 
system.time(extracted.tmax <- lapply(full.rcp45.2020s[25:36], FUN = extract.climateNA)) # open all the ncs 


# combine all together & add up total precipiation
ppts.df <- do.call(cbind, extracted.ppts)
ppts.df$easting <- cov.data.en.df$LON
ppts.df$northing <- cov.data.en.df$LAT
ppts.df$total <- rowSums(ppts.df[,1:12])
  

# get fallspring tmax averages:
tmax.df <- do.call(cbind, extracted.tmax)
tmax.df$easting <- cov.data.en.df$LON
tmax.df$northing <- cov.data.en.df$LAT
tmax.df$tmax_fall_spr_avg <- rowMeans(tmax.df[,c( "Tmax05","Tmax06", "Tmax07","Tmax08" ,"Tmax09", "Tmax10" )])



# need to do the same with the rest of the future climate data..but this is a start


#save as a rds
# climate.2020s <- data.frame(
#                             tmax.df$easting, 
#                             tmax.df$northing, 
#                             tmax.df$tmax_fall_spr_avg, 
#                             ppt.df$total)
# 
# cov.data.en.df
pipo.cores.ll$cov.data$MAP.2020s <- ppts.df$total
pipo.cores.ll$cov.data$tmax.2020s <- tmax.df$tmax_fall_spr_avg

saveRDS(pipo.cores.ll, "pipo.cores.with.2020s.climate.NA.rds")



# do the same for 2050s:
rcp45.2050s.name <- list.files("climateNA_future/NA_ENSEMBLE_rcp45_2050s_Monthly_netCDF/", pattern = ".nc")

full.rcp45.2050s <- paste0(getwd(),"/climateNA_future/NA_ENSEMBLE_rcp45_2050s_Monthly_netCDF/", rcp45.2050s.name)

system.time(extracted.ppts <- lapply(full.rcp45.2050s[1:12], FUN = extract.climateNA)) # open all the ncs 
system.time(extracted.tmax <- lapply(full.rcp45.2050s[25:36], FUN = extract.climateNA)) # open all the ncs 


# combine all together & add up total precipiation
ppts.df <- do.call(cbind, extracted.ppts)
ppts.df$easting <- cov.data.en.df$LON
ppts.df$northing <- cov.data.en.df$LAT
ppts.df$total <- rowSums(ppts.df[,1:12])


# get fallspring tmax averages:
tmax.df <- do.call(cbind, extracted.tmax)
tmax.df$easting <- cov.data.en.df$LON
tmax.df$northing <- cov.data.en.df$LAT
tmax.df$tmax_fall_spr_avg <- rowMeans(tmax.df[,c( "Tmax05","Tmax06", "Tmax07","Tmax08" ,"Tmax09", "Tmax10" )])


pipo.cores.ll$cov.data$MAP.2050s <- ppts.df$total
pipo.cores.ll$cov.data$tmax.2050s <- tmax.df$tmax_fall_spr_avg

saveRDS(pipo.cores.ll, "pipo.cores.with.4.5.2050s.climate.NA.rds")


### and the 2080s:
# do the same for 2050s:
rcp45.2080s.name <- list.files("climateNA_future/NA_ENSEMBLE_rcp45_2080s_Monthly_netCDF/", pattern = ".nc")

full.rcp45.2080s <- paste0(getwd(),"/climateNA_future/NA_ENSEMBLE_rcp45_2080s_Monthly_netCDF/", rcp45.2080s.name)

system.time(extracted.ppts <- lapply(full.rcp45.2080s[1:12], FUN = extract.climateNA)) # open all the ncs 
system.time(extracted.tmax <- lapply(full.rcp45.2080s[25:36], FUN = extract.climateNA)) # open all the ncs 


# combine all together & add up total precipiation
ppts.df <- do.call(cbind, extracted.ppts)
ppts.df$easting <- cov.data.en.df$LON
ppts.df$northing <- cov.data.en.df$LAT
ppts.df$total <- rowSums(ppts.df[,1:12])


# get fallspring tmax averages:
tmax.df <- do.call(cbind, extracted.tmax)
tmax.df$easting <- cov.data.en.df$LON
tmax.df$northing <- cov.data.en.df$LAT
tmax.df$tmax_fall_spr_avg <- rowMeans(tmax.df[,c( "Tmax05","Tmax06", "Tmax07","Tmax08" ,"Tmax09", "Tmax10" )])


pipo.cores.ll$cov.data$MAP.2080s <- ppts.df$total
pipo.cores.ll$cov.data$tmax.2080s <- tmax.df$tmax_fall_spr_avg

saveRDS(pipo.cores.ll, "pipo.cores.with.4.5.2080s.climate.NA.rds")

##################################################################
# FOR RCP 8.5
##################################################################
rcp85.2020s.name <- list.files("climateNA_future/NA_ENSEMBLE_rcp85_2020s_Monthly_netCDF/", pattern = ".nc")

full.rcp85.2020s <- paste0(getwd(),"/climateNA_future/NA_ENSEMBLE_rcp85_2020s_Monthly_netCDF/", rcp85.2020s.name)


system.time(extracted.ppts <- lapply(full.rcp85.2020s[1:12], FUN = extract.climateNA)) # open all the ncs 
system.time(extracted.tmax <- lapply(full.rcp85.2020s[25:36], FUN = extract.climateNA)) # open all the ncs 


# combine all together & add up total precipiation
ppts.df <- do.call(cbind, extracted.ppts)
ppts.df$easting <- cov.data.en.df$LON
ppts.df$northing <- cov.data.en.df$LAT
ppts.df$total <- rowSums(ppts.df[,1:12])


# get fallspring tmax averages:
tmax.df <- do.call(cbind, extracted.tmax)
tmax.df$easting <- cov.data.en.df$LON
tmax.df$northing <- cov.data.en.df$LAT
tmax.df$tmax_fall_spr_avg <- rowMeans(tmax.df[,c( "Tmax05","Tmax06", "Tmax07","Tmax08" ,"Tmax09", "Tmax10" )])



# need to do the same with the rest of the future climate data..but this is a start


#save as a rds
# climate.2020s <- data.frame(
#                             tmax.df$easting, 
#                             tmax.df$northing, 
#                             tmax.df$tmax_fall_spr_avg, 
#                             ppt.df$total)
# 
# cov.data.en.df
pipo.cores.ll$cov.data$MAP.85.2020s <- ppts.df$total
pipo.cores.ll$cov.data$tmax85.2020s <- tmax.df$tmax_fall_spr_avg

saveRDS(pipo.cores.ll, "pipo.cores.with.85.2020s.climate.NA.rds")



# do the same for 2050s:
rcp85.2050s.name <- list.files("climateNA_future/NA_ENSEMBLE_rcp85_2050s_Monthly_netCDF/", pattern = ".nc")

full.rcp85.2050s <- paste0(getwd(),"/climateNA_future/NA_ENSEMBLE_rcp85_2050s_Monthly_netCDF/", rcp85.2050s.name)

system.time(extracted.ppts <- lapply(full.rcp85.2050s[1:12], FUN = extract.climateNA)) # open all the ncs 
system.time(extracted.tmax <- lapply(full.rcp85.2050s[25:36], FUN = extract.climateNA)) # open all the ncs 


# combine all together & add up total precipiation
ppts.df <- do.call(cbind, extracted.ppts)
ppts.df$easting <- cov.data.en.df$LON
ppts.df$northing <- cov.data.en.df$LAT
ppts.df$total <- rowSums(ppts.df[,1:12])


# get fallspring tmax averages:
tmax.df <- do.call(cbind, extracted.tmax)
tmax.df$easting <- cov.data.en.df$LON
tmax.df$northing <- cov.data.en.df$LAT
tmax.df$tmax_fall_spr_avg <- rowMeans(tmax.df[,c( "Tmax05","Tmax06", "Tmax07","Tmax08" ,"Tmax09", "Tmax10" )])


pipo.cores.ll$cov.data$MAP.85.2050s <- ppts.df$total
pipo.cores.ll$cov.data$tmax.85.2050s <- tmax.df$tmax_fall_spr_avg

saveRDS(pipo.cores.ll, "pipo.cores.with.8.5.2050s.climate.NA.rds")


### and the 2080s:
# do the same for 2050s:
rcp85.2080s.name <- list.files("climateNA_future/NA_ENSEMBLE_rcp85_2080s_Monthly_netCDF/", pattern = ".nc")

full.rcp85.2080s <- paste0(getwd(),"/climateNA_future/NA_ENSEMBLE_rcp85_2080s_Monthly_netCDF/", rcp85.2080s.name)

system.time(extracted.ppts <- lapply(full.rcp85.2080s[1:12], FUN = extract.climateNA)) # open all the ncs 
system.time(extracted.tmax <- lapply(full.rcp85.2080s[25:36], FUN = extract.climateNA)) # open all the ncs 


# combine all together & add up total precipiation
ppts.df <- do.call(cbind, extracted.ppts)
ppts.df$easting <- cov.data.en.df$LON
ppts.df$northing <- cov.data.en.df$LAT
ppts.df$total <- rowSums(ppts.df[,1:12])


# get fallspring tmax averages:
tmax.df <- do.call(cbind, extracted.tmax)
tmax.df$easting <- cov.data.en.df$LON
tmax.df$northing <- cov.data.en.df$LAT
tmax.df$tmax_fall_spr_avg <- rowMeans(tmax.df[,c( "Tmax05","Tmax06", "Tmax07","Tmax08" ,"Tmax09", "Tmax10" )])


pipo.cores.ll$cov.data$MAP.85.2080s <- ppts.df$total
pipo.cores.ll$cov.data$tmax.85.2080s <- tmax.df$tmax_fall_spr_avg

saveRDS(pipo.cores.ll, "pipo.cores.with.8.5.2080s.climate.NA.rds")



####################################################################
# plot future climate normals for tree ring plots:
####################################################################
covs <- pipo.cores.ll$cov.data
prism.df <- readRDS("PRISM_non_scaled.rds")
sit.means.tmx <- rowMeans(prism.df$tmax.fallspr)
sit.means.ppt <- rowMeans(prism.df$wintP.wateryr)
hist(sit.means.tmx)
hist(covs$tmax.2020s)

summary.clim.change <- data.frame(rcp45.tmax = covs$tmax.2020s, 
                                  rcp45.map = covs$MAP.2020s, 
                                  rcp85.tmax = covs$tmax85.2020s, 
                                  rcp85.map = covs$MAP.85.2020s, 
                                  # for 2050s
                                  rcp45.tmax.2050 = covs$tmax.2050s, 
                                  rcp45.map.2050 = covs$MAP.2050s, 
                                  rcp85.tmax.2050 = covs$tmax.85.2050s, 
                                  rcp85.map.2050 = covs$MAP.85.2050s,
                                  # for 2080s
                                  rcp45.tmax.2080 = covs$tmax.2080s, 
                                  rcp45.map.2080 = covs$MAP.2080s, 
                                  rcp85.tmax.2080 = covs$tmax.85.2080s, 
                                  rcp85.map.2080 = covs$MAP.85.2080s,
                                  #historical
                                  historical.tmax = sit.means.tmx[1:515], 
                                  historical.map = sit.means.ppt[1:515])

summary.clim.change.m <- melt(summary.clim.change)

summary.clim.change.m$period <- ifelse(summary.clim.change.m$variable %in% c("rcp45.tmax", "rcp45.map", "rcp85.tmax", "rcp85.map"), "2025-2050",
                                       ifelse(summary.clim.change.m$variable %in% c("rcp45.tmax.2050", "rcp45.map.2050","rcp85.tmax.2050", "rcp85.map.2050"),"2050-2075",
                                              ifelse(summary.clim.change.m$variable %in% c("rcp45.tmax.2080","rcp45.map.2080", "rcp85.tmax.2080", "rcp85.map.2080"),"2075-2099", "historical"))) 

summary.clim.change.m$rcp <- ifelse(summary.clim.change.m$variable %in% c("rcp45.tmax", "rcp45.map", "rcp45.tmax.2050", "rcp45.map.2050","rcp45.tmax.2080","rcp45.map.2080"), "RCP 4.5",
                                       ifelse(summary.clim.change.m$variable %in% c("rcp85.tmax", "rcp85.map","rcp85.tmax.2050", "rcp85.map.2050","rcp85.tmax.2080", "rcp85.map.2080"),"RCP 8.5", "historical")) 

summary.clim.change.m$climate <- ifelse(summary.clim.change.m$variable %in% c("rcp45.tmax","rcp85.tmax", "rcp45.tmax.2050","rcp85.tmax.2050","rcp45.tmax.2080","rcp85.tmax.2080","historical.tmax"), "Tmax", "Precipitation") 

future.boxplot.ppt <- ggplot()+geom_boxplot(data = summary.clim.change.m[summary.clim.change.m$climate %in% "Precipitation",], aes(x = period, y = value,  fill = rcp))+ylab("Water year precipitation (mm)")+theme_bw(base_size = 14)+theme(panel.grid = element_blank())#+facet_wrap(~rcp)
future.boxplot.tmax <-ggplot()+geom_boxplot(data = summary.clim.change.m[summary.clim.change.m$climate %in% "Tmax",], aes(x = period, y = value, fill = rcp))+ylab("Fall-spring Tmax (degC)")+theme_bw(base_size = 14)+theme(panel.grid = element_blank())#+facet_wrap(~rcp)

png(height = 3, width = 8, res = 300, units = "in", filename = "boxplots.of.future.climate.png")
cowplot::plot_grid(future.boxplot.ppt, future.boxplot.tmax)
dev.off()

ggplot()+geom_histogram(data = summary.clim.change.m[summary.clim.change.m$variable %in% c("rcp45.tmax", "historical.tmax"),], aes(value, fill = variable))+
  facet_wrap(~variable, ncol = 1)

ggplot()+geom_histogram(data = summary.clim.change.m[summary.clim.change.m$variable %in% c("rcp45.map", "historical.map"),], aes(value, fill = variable))+
  facet_wrap(~variable, ncol = 1)


# easting <- ncvar_get(nc,"easting")
# northing <- ncvar_get(nc,"northing")
# north <- raster(t(northing))
# east <- raster(t(northing))
# 
# 
# #colnames(Tmax) <- northing
# #rownames(Tmax) <- easting
# rast <- raster(t(Tmax))
# plot(rast)
# 
# 
# tmax.stack<-  stack(north, east, rast)
# 
# 
# plot(north)
# 
# northing
# 
# 
# t <- ncvar_get(nc,"easting")
# t
# tunits <- ncatt_get(nc,"time","units")
# nt <- dim(t)
# nt
# 
# dname <- "pr"
# 
# tmp_array <- ncvar_get(nc,dname)
# dlname <- ncatt_get(nc,dname,"long_name")
# dunits <- ncatt_get(nc,dname,"units")
# fillvalue <- ncatt_get(nc,dname,"_FillValue")
# dim(tmp_array)
# 
# title <- ncatt_get(nc,0,"title")
# institution <- ncatt_get(nc,0,"institution")
# datasource <- ncatt_get(nc,0,"source")
# references <- ncatt_get(nc,0,"references")
# history <- ncatt_get(nc,0,"history")
# Conventions <- ncatt_get(nc,0,"Conventions")
# 
# 
# 
# 
# 
# #nc <- nc_open(file.path(path.guess, "LPJ-GUESS_annual.nc"))
# #summary(nc$var)
# 
# nc.out <- list()
# nc.out$lat <- ncvar_get(nc, "latitude")
# nc.out$lon <- ncvar_get(nc, "longitude")
# nc.out$Time <- ncvar_get(nc, "time") 
# nc.out$pr <- ncvar_get(nc, "pr")
# #for(v in names(nc$var)){
# #  nc.out[[v]] <- ncvar_get(nc, v)  
# #}
# 
# 
# as_date(nc.out$Time[2])
# dim(nc.out$pr)
# 
# 
# for(y in 1:dim(nc.out$pr)[2]){
#   print(paste0(" ---- Lat: ", y, " ---- "))
#   dat.temp <- stack(data.frame(nc.out$pr[,y,,1]))
#   names(dat.temp) <- c("pr", "Year")
#   dat.temp$Year <- as_date(nc.out$Time[y])
#   dat.temp$lat  <- nc.out$lat[y]
#   dat.temp$lon  <- nc.out$lon
#   
#   if(y==1) dat.pr <- dat.temp else dat.pr <- rbind(dat.pr, dat.temp)
# }
# 


#dat.pr <- dat.pr[complete.cases(dat.pr),]
summary(dat.pr)
#mean(dat.pr$NEE.yr, na.rm=T)
#max(dat.pr$NEE.yr, na.rm=T)

#mean(dat.pr$NEE, na.rm=T)
dat.pr$yr <- year(dat.pr$Year)
dat.pr$mo <- month(dat.pr$Year)
summary(dat.pr[dat.pr$yr == 2092,])
# Graphing
ggplot(data=dat.pr[dat.pr$yr == 2092,]) +
  #facet_grid(mo~.) +
  geom_raster(aes(x=lon, y=lat, fill=pr)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("test_pr") +
  coord_equal(ratio=1)