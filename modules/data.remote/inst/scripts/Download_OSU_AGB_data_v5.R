####################################################################################################
#
#        Simple download and extraction script for NASA CMS ABG data products
#        Data source:
#
#
#
#    --- Last updated:  09.04.2018 By Shawn Serbin <sserbin@bnl.gov>
####################################################################################################


#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
dlm <- .Platform$file.sep # <--- What is the platform specific delimiter?
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### get libs
if (!("PEcAn.utils" %in% installed.packages()[, "Package"])) {
  devtools::install_github("pecanproject/pecan", ref = "develop", subdir = "base/utils")
}
library(PEcAn.utils)

# other packages that may need to be used - also may need to add to data.land
library(raster)
library(sp)
library(rgdal)
library(spatial.tools)
library(ggplot2)
library(plyr)  # needed?
#library(plotrix)
library(foreach)
#library(magrittr)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
URL <- "ftp://islay.ceoas.oregonstate.edu/cms"
dataset_version <- "v1"
target_dataset <- "biomassfiaald"
file_ext <- ".zip"
target_directory <- file.path("/data2/RS_GIS_Data/NASA_CMS_data/OSU_AGB/")
if (! file.exists(target_directory)) dir.create(target_directory,recursive=TRUE)
start_year <- 1984
end_year <- 2016
inParallel <- TRUE
ncores = if (inParallel) parallel::detectCores() else NULL
plot_results <- TRUE
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
target_years <- seq(start_year,end_year,1)
med_files <- paste0(target_dataset,"_",target_years,"_median",file_ext)
stdv_files <- paste0(target_dataset,"_",target_years,"_stdv",file_ext)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Download ABG data
cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)

# get median data
med_urls <- paste(URL,dataset_version,target_dataset,"median",med_files,sep="/")
PEcAn.logger::logger.info("*** Downloading AGB median data")
foreach::foreach(i=1:length(med_urls)) %dopar% try(utils::download.file(med_urls[i], file.path(target_directory,med_files[i])))
rm(i)

# get stdv data
stdv_urls <- paste(URL,dataset_version,target_dataset,"stdv",stdv_files,sep="/")
PEcAn.logger::logger.info("*** Downloading AGB stdv data")
foreach::foreach(j=1:length(stdv_urls)) %dopar% try(utils::download.file(stdv_urls[j], file.path(target_directory,stdv_files[j])))
rm(j)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## unpack files
zip_files <- list.files(file.path(target_directory), pattern = "*.zip", full.names = TRUE)
PEcAn.logger::logger.info("*** Unzipping downloaded data")
foreach::foreach(k=1:length(zip_files)) %dopar% try(utils::unzip(file.path(zip_files[k]),
                                                                 files = NULL, list = FALSE, overwrite = TRUE,
                                                                 junkpaths = FALSE, 
                                                                 exdir = file.path(path.expand(target_directory)),
                                                                 unzip = getOption("unzip"), setTimes = FALSE))
rm(k)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## extract pixel values

con <- PEcAn.DB::db.open(
   list(user='bety', password='bety', host='localhost',
   dbname='bety', driver='PostgreSQL',write=TRUE))
site_ID <- c(2000000023,1000025731)  # US-CZ3, US-SSH
sites <- list(PEcAn.DB::query.site(site_ID[1],con),PEcAn.DB::query.site(site_ID[2],con))

coords_LL <- NULL
j <- 1
for (i in seq_along(1:length(sites))) {
  if (j==1) {
    coords_latlong <- data.frame(cbind(sites[[i]]$lon,sites[[i]]$lat))
    names(coords_latlong) <- c("Longitude","Latitude")
  } else {
    coords_latlong[j,] <- rbind(sites[[i]]$lon,sites[[i]]$lat)
  }
  j <- j+1
}
coords_latlong
coords_latlong <- sp::SpatialPoints(coords_latlong)
proj4string(coords_latlong) <- CRS("+init=epsg:4326")

biomass_median <- lapply(list.files(file.path(target_directory), pattern = "*median.tif$", full.names = TRUE), raster)
biomass_stdv <- lapply(list.files(file.path(target_directory), pattern = "*stdv.tif$", full.names = TRUE), raster)
coords_AEA <- sp::spTransform(coords_latlong,crs(raster::raster(biomass_median[[1]])))

biomass_median_stack <- raster::stack(biomass_median)
biomass_stdv_stack <- raster::stack(biomass_stdv)
agb_median_pixel <- raster::extract(x = biomass_median_stack, y = coords_AEA, buffer=NULL, fun=NULL, df=FALSE)
agb_median_pixel <- data.frame(site_ID, agb_median_pixel)
agb_stdv_pixel <- raster::extract(x = biomass_stdv_stack, y = coords_AEA, buffer=NULL, fun=NULL, df=FALSE)
agb_stdv_pixel <- data.frame(site_ID, agb_stdv_pixel)
point_list <- list(median_AGB=list(agb_median_pixel), stdv_AGB=list(agb_stdv_pixel))

#write.csv(x = point_list, file = file.path(target_directory,'example_AGB_output.csv'), row.names = FALSE)
save("point_list",file = file.path(target_directory,'example_AGB_output.RData'))

if (plot_results) {
  
  PEcAn.logger::logger.info("Generating plot of results")
  ## format for plotting in ggplot2
  formatted_data <- data.frame(reshape::melt(agb_median_pixel, id=c("site_ID")))
  formatted_data$AGB_Stdv <- reshape::melt(agb_stdv_pixel, id=c("site_ID"))$value
  formatted_data$Year <- substr(gsub("[^0-9]","",formatted_data$variable),1,8)
  formatted_data <- formatted_data[,c(1,3:5)]
  names(formatted_data) <- c("site_ID","AGB_med","AGB_stdv","Year")
  #formatted_data
  
  png(file=file.path(target_directory,'Example_AGB_pixel_extraction.png'),height=1300,width=3900, res=310)  
  par(mfrow=c(1,1), mar=c(4.5,5.7,0.3,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  ggplot2::ggplot(data=formatted_data, aes(x = Year, y = AGB_med)) + geom_point(size=2) + 
    geom_errorbar(aes(x=Year, ymin=AGB_med-AGB_stdv, ymax=AGB_med+AGB_stdv), width=0.25) + facet_wrap(~site_ID) +
    theme(axis.text=element_text(size=18),legend.text=element_text(size=18),axis.title.y=element_text(size=18,face="bold"),
          axis.text.x=element_text(size=12, angle=90, vjust = 0.3))
  dev.off()
  
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF
