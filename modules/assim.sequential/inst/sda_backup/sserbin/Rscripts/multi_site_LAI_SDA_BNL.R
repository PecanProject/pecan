####################################################################################################
#
#     Single site LAI SDA
#
#
#    --- Last updated:  03.27.2019 By Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#---------------- Load required libraries ---------------------------------------------------------#
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
library(doParallel)

extract_LAI <- TRUE #TRUE/FALSE
run_SDA <- TRUE #TRUE/FALSE

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## set run options, some of these should be tweaked or removed as requirements
work_dir <- "/data/sserbin/Modeling/sipnet/NASA_CMS_AGB_LAI"
setwd(work_dir)  # best not to require setting wd and instead just providing full paths in functions

# grab multi-site XML file
settings <- read.settings("XMLs/pecan_MultiSite_LAI_SDA.xml")

# grab observation IDs from settings file
observation <- c()
for (i in seq_along(1:length(settings$run))) {
  command <- paste0("settings$run$settings.",i,"$site$id")
  obs <- eval(parse(text=command))
  observation <- c(observation,obs)
}


# delete an old run
unlink(c('run','out','SDA'),recursive = T)

# what is this step for????  is this to get the site locations for the map??
if ("MultiSettings" %in% class(settings)) site.ids <- settings %>% 
  map(~.x[['run']] ) %>% map('site') %>% map('id') %>% unlist() %>% as.character()

# sample from parameters used for both sensitivity analysis and Ens
get.parameter.samples(settings, 
                      ens.sample.method = settings$ensemble$samplingspace$parameters$method)  
## Aside: if method were set to unscented, would take minimal changes to do UnKF
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Prepare observational data - still very hacky here

# where to put MODIS LAI data?
data_dir <- "/data/sserbin/Modeling/sipnet/NASA_CMS_AGB_LAI/modis_lai_data"
parameters <- settings$run

# get MODIS data
bety <- list(user=settings$database$bety$user, password=settings$database$bety$password, 
             host=settings$database$bety$host,
             dbname='bety', driver='PostgreSQL',write=TRUE)
con <- PEcAn.DB::db.open(bety)
bety$con <- con

suppressWarnings(site_qry <- glue::glue_sql("SELECT *, ST_X(ST_CENTROID(geometry)) AS lon, 
                            ST_Y(ST_CENTROID(geometry)) AS lat FROM sites WHERE id IN ({ids*})", 
                           ids = observation, .con = con))
suppressWarnings(qry_results <- DBI::dbSendQuery(con,site_qry))
suppressWarnings(qry_results <- DBI::dbFetch(qry_results))
site_IDs <- qry_results$id
site_names <- qry_results$sitename
site_coords <- data.frame(cbind(qry_results$lon, qry_results$lat))
names(site_coords) <- c("Longitude","Latitude")

#extract lai using call_MODIS for the lat/lon per site  and dates
if (extract_LAI) {
  modis_data <- data.frame()
  cl <- parallel::makeCluster(5, outfile="")
  registerDoParallel(cl)
  modis_data <- foreach(i=1:nrow(site_coords)) %dopar% PEcAn.data.remote::call_MODIS(product = "MOD15A2H", 
                                                                     band = "Lai_500m", start_date = "2001001", 
                                                                     end_date = "2010365", lat = site_coords$Latitude[i], 
                                                                     lon = site_coords$Longitude[i],
                                                                     size = 0, band_qc = "FparLai_QC", 
                                                                     band_sd = "LaiStdDev_500m", 
                                                                     package_method = "MODISTools")
  
  stopCluster(cl)
  modis_data <- do.call(rbind.data.frame, modis_data)
  
  # modis_data <- data.frame()
  # for (i in 1:length(observation)) {
  #   print(paste("extracting site: ", observation[i], sep = ""))
  #   data <- PEcAn.data.remote::call_MODIS(lat = site_coords[i,2], lon = site_coords[i,1], 
  #                                        start_date = "2001001", end_date = "2010365", 
  #                                        size = 0, product = "MOD15A2H", band = "Lai_500m", 
  #                                        band_qc = "", band_sd = "LaiStdDev_500m", package_method = "MODISTools")
  #   modis_data <- rbind(modis_data, data)
  # }
  # output resuls of call_MODIS
  save(modis_data, file = file.path(data_dir,'modis_lai_output.RData'))
} else {
  load(file = file.path(data_dir,'modis_lai_output.RData'))
}

# find peaks
peak_lai <- data.frame()
years <- unique(year(as.Date(modis_data$calendar_date, "%Y-%m-%d")))
#site_ll <- data.frame(cbind(lon=unique(modis_data$lon),lat=unique(modis_data$lat)))
site_ll <- data.frame(cbind(lat=unique(modis_data$lat),lon=unique(modis_data$lon)))
for (i in 1:length(years)) {
  year <- years[i]
  g <- grep(modis_data$calendar_date, pattern = year)
  d <- modis_data[g,]
  for (j in 1:length(site_IDs)) {
    pixel <- filter(d, lat == site_ll[j,1] & lon == site_ll[j,2])
    
    # using peak
    peak <- pixel[which(pixel$data == max(pixel$data, na.rm = T)),][1,]
    
    # using mean
    #mn_data <- mean(pixel$data, na.rm = T)
    #mn_sd <- mean(pixel$sd, na.rm = T)
    #peak <- pixel[1,]
    #peak$data <- mn_data
    #peak$sd <- mn_sd
    
    
    peak$calendar_date = paste("Year", year, sep = "_")
    peak$tile <- site_names[j]
    #peak$tile <- site_IDs[j]
    peak_lai <- rbind(peak_lai, peak) 
    }
}

# sort the data by site so the correct values are placed into the resized data frames below.
peak_lai <- peak_lai[order(peak_lai$tile), ]

# separate data into hotdog style dataframes with row == site and columns = info/data for each site
median_lai <- cbind(site_IDs, site_names, as.data.frame(matrix(unlist(t(peak_lai$data)), byrow = T, length(site_IDs), length(years))))
colnames(median_lai) <- c("Site_ID", "Site_Name", unique(peak_lai$calendar_date))

stdv_lai <- cbind(site_IDs, site_names, as.data.frame(matrix(unlist(t(peak_lai$sd)), byrow = T, length(site_IDs), length(years))))
colnames(stdv_lai) <- c("Site_ID", "Site_Name", unique(peak_lai$calendar_date))

# convert to list
point_list <- list()
point_list$median_lai <- list(median_lai)
point_list$stdv_lai <- list(stdv_lai)
point_list

point_list$median_lai <- point_list$median_lai[[1]]
point_list$stdv_lai <- point_list$stdv_lai[[1]]

#point_list$median_lai <- point_list$median_lai[[1]] %>% filter(Site_ID %in% site.ids)
#point_list$stdv_lai <- point_list$stdv_lai[[1]] %>% filter(Site_ID %in% site.ids)
#site.order <- sapply(site.ids,function(x) which(point_list$median_lai$Site_ID %in% x)) %>%
#  as.numeric() %>% na.omit()
#point_list$median_lai <- point_list$median_lai[site.order,]
#point_list$stdv_lai <- point_list$stdv_lai[site.order,]
#point_list

site.order <- sapply(site.ids,function(x) which(point_list$median_lai$Site_ID %in% x)) %>%
  as.numeric() %>% na.omit()
point_list$median_lai <- point_list$median_lai[site.order,]
point_list$stdv_lai <- point_list$stdv_lai[site.order,]

# truning lists to dfs  for both mean and cov
date.obs <- strsplit(names(point_list$median_lai),"_")[3:length(point_list$median_lai)] %>% map_chr(~.x[2]) %>% paste0(.,"/07/15") 

obs.mean <- names(point_list$median_lai)[3:length(point_list$median_lai)] %>%
  map(function(namesl){
    ((point_list$median_lai)[[namesl]] %>% 
       map(~.x %>% as.data.frame %>% `colnames<-`(c('LAI'))) %>% 
       setNames(site.ids[1:length(.)])
    )
  }) %>% setNames(date.obs)

obs.cov <-names(point_list$stdv_lai)[3:length(point_list$median_lai)] %>%
  map(function(namesl) {
    ((point_list$stdv_lai)[[namesl]] %>%
       map( ~ (.x) ^ 2 %>% as.matrix()) %>%
       setNames(site.ids[1:length(.)]))
    
  }) %>% setNames(date.obs)

# check input data - after creating list of lists
PEcAn.assim.sequential::Construct.R(site.ids, "LAI", obs.mean[[1]], obs.cov[[1]])
PEcAn.assim.sequential::Construct.R(site.ids, "LAI", obs.mean[[10]], obs.cov[[10]])
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## generate new settings object
new.settings <- PEcAn.settings::prepare.settings(settings)
# Write pecan.CHECKED.xml
PEcAn.settings::write.settings(new.settings, outputfile = "pecan.CHECKED.xml")
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Run SDA
if (run_SDA) {
  sda.enkf.multisite(new.settings, obs.mean = obs.mean ,obs.cov = obs.cov,
                     control=list(trace=T,
                                  FF=F,
                                  interactivePlot=T,
                                  TimeseriesPlot=T,
                                  BiasPlot=T,
                                  plot.title="LAI SDA, uniform sampling",
                                  facet.plots=T,
                                  debug=T,
                                  pause=F))
} else {
  print("*** Not running SDA ***")
}

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Wrap up
# Send email if configured
#if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
#  sendmail(settings$email$from, settings$email$to,
#           paste0("SDA workflow has finished executing at ", base::date()))
#}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF
