## SMAP data prep cron script

####  Libraries and configs
#BiocManager::install("rhdf5")
library(smapr)
library(raster)
library(hdf5r)
library(magrittr)
### Run once: smapr::set_smap_credentials(username,password)  
### using login for https://urs.earthdata.nasa.gov/

smap_dir = "/projectnb/dietzelab/dietze/hf_landscape_SDA/SMAP_data"
smap_file = file.path(smap_dir, "SMAP.csv")
smap_download_dir = file.path(smap_dir,"raw")

## sites from pecan.xml
set = readRDS("/projectnb/dietzelab/dietze/hf_landscape_SDA/test01/pecan.RDS")
sites <- set$run %>% purrr::map('site') %>% 
  purrr::map_dfr(~c(.x[['id']],.x[['lon']],.x[['lat']]) %>%as.numeric)%>% 
  t %>% `colnames<-`(c("id","lon","lat")) %>% as.data.frame()
site.id = 758 ## NEON HARV
lat = mean(sites$lat)
lon = mean(sites$lon)


### DEFINE HELPER FUNCTIONS

extract_SMAP <- function(file, name = c("Soil_Moisture_Retrieval_Data_AM","Soil_Moisture_Retrieval_Data_PM"), in_memory = FALSE){
  ## debugging version of smapr::extract_smap
#  smapr:::validate_data(data)
#  file <- smapr:::local_h5_paths(data)
  n_files <- length(file)
  if(n_files > 1) {
    print("currently not supporting multiple time points")
    return()
  }
  rasters <- vector("list", length = 2)
  for (i in 1:2) {
    h5_in <- rhdf5::h5read(file, name[i])
    if (smapr:::is_cube(h5_in)) {
      rasters[[i]] <- smapr:::rasterize_cube(h5_in, file, name)
    } else {
#      r <- smapr:::rasterize_matrix(h5_in, file, name)
      rasters[[i]] <- rasterize_list(h5_in, file, name[i])
    }
  }
#  output <- smapr:::bundle_rasters(rasters, data, in_memory)
#  output
 rasters 
}

rasterize_list <- function (h5_in, file, name) 
{
  fill_value <- smapr:::find_fill_value(file, name)
  if(name == "Soil_Moisture_Retrieval_Data_PM"){
    matrix    <- h5_in[["soil_moisture_pm"]]
  } else {
    matrix <- h5_in[["soil_moisture"]]
  }
  matrix[matrix == fill_value] <- NA
  raster_layer <- raster(t(matrix))
  raster_layer <- smapr:::project_smap(file, raster_layer)
  raster_layer
}

ave_smp <- function(am,pm){
  a = apply(cbind(am,pm),1,mean,na.rm=TRUE)
  a[is.nan(a)] = NA
  a
}

### Extraction workflow 

## Determine dates to be processed
smap_dates = Sys.Date()
smap_dates = seq(Sys.Date(),Sys.Date()-lubridate::days(5),by="-1 day")

## Determine dates already downloaded
prev_download  = dir(smap_download_dir,"h5$")
prev_dates     = as.Date(substr(prev_download,14,21),format = "%Y%m%d")

## Download raw h5 files 
status = data.frame(date  = smap_dates,
                    file  = rep(NA,length(smap_dates)),
                    avail = rep(NA,length(smap_dates)))
for(t in seq_along(smap_dates)){
  print(smap_dates[t])

  ## check if we've downloaded the raw data for this date already
  if(status$date[t] %in% prev_dates){
    
    #add to list of filenames
    status$file[t] = prev_download[prev_dates == status$date[t]]
    status$avail[t]  = TRUE
    
  } else {
    ## check if new data is available  
    available_data <- find_smap(id = "SPL3SMP", date = smap_dates[t], version = 8)
    if(is.na(available_data$dir)){
      status$avail[t] = FALSE
      next
    }
    ### if multiple file versions, assume we want the last (latest?) one
    available_data = available_data[nrow(available_data),]
    
    downloads <- download_smap(available_data,directory = smap_download_dir)
    status$file[t]  = paste0(downloads$name,".h5")
    status$avail[t] = TRUE
    
  }
}

## eliminate dates without data
status = status %>% dplyr::filter(avail == TRUE)  

## set up storage
if(file.exists(smap_file)){
  SMAP_CSV = read.csv(smap_file)
} else {
  SMAP_CSV <- matrix(NA, 0, 6) %>% `colnames<-`(c("date", "site_id", "lat", "lon", "smp", "sd")) %>% as.data.frame()
}


for(t in seq_along(status$date)){
  print(status$date[t])

  ## determine what sites have already been processed
  SMAP_done = SMAP_CSV[SMAP_CSV$site_id %in% site.id & SMAP_CSV$date == status$date[t],]
  if(nrow(SMAP_done) > 0){
    site.todo = !(site.id %in% SMAP_done$site_id)
    if(all(!site.todo)){ next } ## done, skip to the next year
  } else {
    ## no sites already processed
    site.todo = rep(TRUE,length(site.id))
  }
  
  
  ## load data as a raster, returns a list of length 2 for AM and PM passes
  sm_raster <- extract_SMAP(file.path(smap_download_dir,status$file[t]))

  ## extract missing site days
  am = raster::extract(sm_raster[[1]],y = data.frame(lat=lat[site.todo],lon=lon[site.todo]),cellnumbers=TRUE)[,"layer"]
  pm = raster::extract(sm_raster[[2]],y = data.frame(lat=lat[site.todo],lon=lon[site.todo]),cellnumbers=TRUE)[,"layer"]
  smp = ave_smp(am,pm)
  ## a more sophisticated way of doing this would be to use the cellnumbers to detect duplicate extractions
  sd = rep(0.04,length(smp)) ## using same default as Dongchen, which comes from Dan G.
  SMAP_CSV <- rbind(SMAP_CSV, tibble::tibble(date=status$date[t], site_id = site.id[site.todo], lat, lon, smp, sd))
   
}

## save  
utils::write.csv(SMAP_CSV, file = file.path(smap_dir, "SMAP.csv"), row.names = F)
