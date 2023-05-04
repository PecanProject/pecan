# ----------------------------------------------------------------------
#------------------------------------------ Load required libraries-----
# ----------------------------------------------------------------------
library("PEcAn.all")
library("PEcAn.utils")
library("PEcAn.data.remote")
library("PEcAnAssimSequential")
library("REddyProc")
library("tidyverse")
library("furrr")
library("R.utils")
library("dynutils")
library('nimble')
library("sp")
library("sf")
library("lubridate")
#plan(multisession)


# ----------------------------------------------------------------------------------------------
#------------------------------------------Prepared SDA Settings -----
# ----------------------------------------------------------------------------------------------
# tmp = commandArgs(trailingOnly = TRUE)
# if(length(tmp) < 6){
#   logger.severe("Missing required arguments")
# }
#forecastPath points to the folder where unconstrained forecast runs can be found
#forecastPath <- "/projectnb/dietzelab/ahelgeso/Site_Outputs/Harvard/Fixed_PAR/"
#SDApath points to the folder where SDA forecast runs can be found
SDApath <- "/projectnb/dietzelab/ahelgeso/SDA/HF_SDA_Output/Fixed_PAR"
#SDApath <- tmp[1]
#manually set to previous run settings$info$date it creates the filepath to previous run
#when you run with write to BETY = FALSE the system uses the system date/time as the unique folder name for runs
next.oldir <- "2022-09-23-11-49"
#next.oldir <- tmp[2]
#outputPath points to location where you would like to save SDA output note this path could match SDApath but does not need to
outputPath <- "/projectnb/dietzelab/ahelgeso/SDA/HF_SDA_Output/Fixed_PAR"
#outputPath <- tmp[3]
#settingsPath points to location where multisite xml can be found
settingsPath <- "/projectnb/dietzelab/ahelgeso/pecan/modules/assim.sequential/inst/Site_XMLS/testingMulti_HF.xml"
#settingsPath <- tmp[4]
#to manually change start date 
runDays <- seq(as.Date("2021-07-28"), as.Date("2021-07-29"), by="days")
#runDays <- seq(as.Date(tmp[5]), as.Date(tmp[6]), by="days")

#------------------------------------------------------------------------------------------------
#------------------------------------------ Preparing the pecan xml -----------------------------
#------------------------------------------------------------------------------------------------
for (s in 1:length(runDays)) {
#restart list will store the full filepath to previous runs and when to start SDA cut 
restart <- list()
setwd(outputPath)
#set sda.start
sda.start <- as.Date(runDays[s])
#sda.start <- as.Date("2021-07-15")

#reading xml
settings <- read.settings(settingsPath)

#grab site info
site_info <- list(
  site_id = settings$run$site$id,
  site_name = settings$run$site$name,
  lat = settings$run$site$lat,
  lon = settings$run$site$lon,
  time_zone = "UTC")

#grab old.dir filepath from previous SDA run
sda.runs <- list.files(SDApath, full.names = TRUE, pattern = paste0("PEcAn_", next.oldir))
#add filpath to restart list
restart$filepath <- sda.runs

#connecting to DB
con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)
on.exit(db.close(con))

# #query database for previous forecast run (i.e. t=0)
# query.run <- paste0("SELECT * FROM runs WHERE site_id =", site_info$site_id)
# run <- PEcAn.DB::db.query(query.run, con)
# #filter for sda.start
# run <- dplyr::filter(run, start_time == as.Date(sda.start -1))
# daydiff <- difftime(Sys.time(), run$created_at, units = "days")
# runday <- which(min(daydiff) == daydiff)
# startday <- run$created_at[runday]
# run <- dplyr::filter(run, as.Date(created_at) == as.Date(startday))
# run <- dplyr::filter(run, !is.na(finished_at))
# #add filter for model
# query.ens <- paste0("SELECT * FROM ensembles WHERE id =", run$ensemble_id)
# ens <- PEcAn.DB::db.query(query.ens, con)
# #now that we have the workflow id for forecast run we can close connection to BETY
# PEcAn.DB::db.close(con)
# #add filepath to restart object, this is where SDA will look for runs for t=1
# restart$filepath <- paste0(forecastPath, "PEcAn_", ens$workflow_id, "/")
# #restart$filepath <- "/projectnb/dietzelab/ahelgeso/Site_Outputs/Harvard/FluxPaper/PEcAn_1000022323/"
# #check if all ensemble members are present
# ensPresent <- list()
# for(k in 1:length(run$ensemble_id)){
#   ensPresent[[k]] <- file.exists(paste0(restart$filepath, "out/", run$id[k], "/2021.nc"))
# }
# if(FALSE %in% ensPresent){
#   next
# }

#set met.start & met.end
met.start <- sda.start - 1
met.end <- met.start + lubridate::days(35)

# --------------------------------------------------------------------------------------------------
#---------------------------------------------- LAI DATA -------------------------------------
# --------------------------------------------------------------------------------------------------


  lai <- call_MODIS(outdir = NULL,
                    var = 'lai', 
                    site_info = site_info, 
                    product_dates = c(paste0(lubridate::year(met.start), strftime(met.start, format = "%j")),paste0(lubridate::year(met.end), strftime(met.end, format = "%j"))),
                    run_parallel = TRUE, 
                    ncores = NULL, 
                    product = "MOD15A2H", 
                    band = "Lai_500m",
                    package_method = "MODISTools", 
                    QC_filter = TRUE,
                    progress = TRUE)
  
#filter for good resolution data
  lai <- lai %>% filter(qc == "000") 
#filter for lai that matches sda.start
  lai <- lai %>% filter(calendar_date == sda.start)

  if(dim(lai)[1] < 1){
    lai = data.frame(calendar_date = sda.start, site_id = site_info$site_id, data = NA)
    PEcAn.logger::logger.warn(paste0("MODIS mean Data not available for these dates, initialzing NA"))
  }

  lai_sd <- call_MODIS(outdir = NULL,
                       var = 'lai', 
                       site_info = site_info, 
                       product_dates = c(paste0(lubridate::year(met.start), strftime(met.start, format = "%j")),paste0(lubridate::year(met.end), strftime(met.end, format = "%j"))),
                       run_parallel = TRUE, 
                       ncores = NULL, 
                       product = "MOD15A2H", 
                       band = "LaiStdDev_500m",
                       package_method = "MODISTools", 
                       QC_filter = TRUE,
                       progress = TRUE)
 
#filter for good resolution data
  lai_sd <- lai_sd %>% filter(qc == "000")
#filter for lai.sd that matches sda.start
  lai_sd <- lai_sd %>% filter(calendar_date == sda.start)
  
  if(dim(lai_sd)[1] < 1){
    lai_sd = data.frame(calendar_date = sda.start, site_id = site_info$site_id, data = NA)
    PEcAn.logger::logger.warn(paste0("MODIS standard deviation Data not available for these dates, initialzing NA"))
  }

#build obs mean/cov matrix for LAI
  #add NA obs for 1 day after LAI obs available
  na.date <- as.Date(sda.start + 1)
  na.date <- as.character(na.date)
  obs.mean <- data.frame(date = c(lai$calendar_date, na.date), site_id = c(lai$site_id, lai$site_id), lai = c(lai$data, NA))
  obs.mean$date = as.character(obs.mean$date, stringsAsFactors = FALSE)
  obs.mean <- split(obs.mean, obs.mean$date)
  
  date.obs <- names(obs.mean)
  
  obs.mean <- purrr::map(
    names(obs.mean),
    function(namesl){
      split(
        obs.mean[[namesl]],
        obs.mean[[namesl]]$site_id) %>%
        purrr::map(
          ~.x[3] %>%
            stats::setNames(c("LAI")) %>%
            `row.names<-`(NULL))
    }
  ) %>% stats::setNames(date.obs)
  
  # #remove NA data as this will crash the SDA. Removes rown numbers (may not be nessesary)
  # names = date.obs
  # for (name in names){
  #   for (site in names(obs.mean[[name]])){
  #     na_index = which(!(is.na(obs.mean[[ name]][[site]])))
  #     colnames = names(obs.mean[[name]][[site]])
  #     #we have some records that are not NA
  #     if (length(na_index) > 0){
  #       obs.mean[[name]][[site]] = obs.mean[[name]][[site]][na_index]
  #     }else if(length(na_index) == 0){#we don't have any observations (they are all NAs), we then just remove the whole site
  #       obs.mean[[name]][[site]] <- NULL
  #     }
  #   }
  # }
  
  obs.cov <- data.frame(date = c(lai_sd$calendar_date, na.date), site_id = c(lai_sd$site_id, lai_sd$site_id), lai = c(lai_sd$data, NA))
  obs.cov$date = as.character(obs.cov$date, stringsAsFactors = FALSE)
  obs.cov <- split(obs.cov, obs.cov$date)
  
  obs.cov <- purrr::map(
    names(obs.cov),
    function(namesl){
      purrr::map(
        split(
          obs.cov[[namesl]],
          obs.cov[[namesl]]$site_id),
        ~.x[3]^2 %>%
          unlist %>%
          diag(nrow = 2, ncol = 2))
    }
  ) %>% stats::setNames(date.obs)
  
  
  # names = date.obs
  # for (name in names){
  #   for (site in names(obs.cov[[name]])){
  #     #if we don't have any observation (diag(cov)==NA) then we remove the whole site
  #     if(length(which(!is.na(diag(obs.cov[[name]][[site]])))) == 0){
  #       obs.cov[[name]][[site]] <- NULL
  #       next
  #     }
  #     #else we do have some records
  #     bad = which(apply(obs.cov[[name]][[site]], 2, function(x) any(is.na(x))) == TRUE)
  #     if (length(bad) > 0){
  #       obs.cov[[name]][[site]] = obs.cov[[name]][[site]][,-bad]
  #       if (is.null(dim(obs.cov[[name]][[site]]))){
  #         obs.cov[[name]][[site]] = obs.cov[[name]][[site]][-bad]
  #       } else {
  #         obs.cov[[name]][[site]] = obs.cov[[name]][[site]][-bad,]
  #       }
  #     }
  #   }
  # }
  #add start.cut to restart list
  restart$start.cut <- lubridate::as_datetime(min(lai$calendar_date))
  restart$start.cut <- format(restart$start.cut, "%Y-%m-%d %H:%M:%S", tz = "EST")
  

#-----------------------------------------------------------------------------------------------
#------------------------------------------ Fixing the settings --------------------------------
#-----------------------------------------------------------------------------------------------
#Using the found dates to run - this will help to download mets
settings$run$site$met.start <- as.character(met.start)
settings$run$site$met.end <- as.character(met.end)

# Setting dates in assimilation tags - This will help with preprocess split in SDA code
settings$state.data.assimilation$start.date <-as.character(sda.start)
sda.end <- max(names(obs.mean))
settings$state.data.assimilation$end.date <-as.character(sda.end)

# --------------------------------------------------------------------------------------------------
#---------------------------------------------- PEcAn Workflow -------------------------------------
# --------------------------------------------------------------------------------------------------
#info
settings$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"))
next.oldir <- paste0(format(Sys.time(), "%Y-%m-%d-%H-%M"))
#Update/fix/check settings. Will only run the first time it's called, unless force=TRUE
settings <- PEcAn.settings::prepare.settings(settings, force = TRUE)
settings$host$rundir <- settings$rundir
settings$host$outdir <- settings$modeloutdir
settings$host$folder <- settings$modeloutdir
setwd(settings$outdir)
#Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")
# start from scratch if no continue is passed in
statusFile <- file.path(settings$outdir, "STATUS")
if (length(which(commandArgs() == "--continue")) == 0 && file.exists(statusFile)) {
  file.remove(statusFile)
}

#manually add in clim files 
con <-PEcAn.DB::db.open(settings$database$bety)
on.exit(db.close(con), add = TRUE)

input_check <- PEcAn.DB::dbfile.input.check(
  siteid= site_info$site_id %>% as.character(),
  startdate = met.start %>% as.Date,
  enddate = NULL,
  parentid = NA,
  mimetype="text/csv",
  formatname="Sipnet.climna",
  con = con,
  hostname = PEcAn.remote::fqdn(),
  pattern = NULL, 
  exact.dates = TRUE,
  return.all=TRUE
)

#If INPUTS already exists, add id and met path to settings file

if(length(input_check$id) > 0){
  #met paths 
  clim_check = list()
  for(i in 1:length(input_check$file_path)){
    
    clim_check[[i]] <- file.path(input_check$file_path[i], input_check$file_name[i])
  }#end i loop for creating file paths 
  #ids
  index_id = list()
  index_path = list()
  for(i in 1:length(input_check$id)){
    index_id[[i]] = as.character(input_check$id[i])#get ids as list
    
  }#end i loop for making lists
  names(index_id) = sprintf("id%s",seq(1:length(input_check$id))) #rename list
  names(clim_check) = sprintf("path%s",seq(1:length(input_check$id)))
  
  settings$run$inputs$met$id = index_id
  settings$run$inputs$met$path = clim_check
}else{PEcAn.utils::logger.error("No met file found")}
#settings <- PEcAn.workflow::do_conversions(settings, T, T, T)

if(is_empty(settings$run$inputs$met$path) & length(clim_check)>0){
  settings$run$inputs$met$id = index_id
  settings$run$inputs$met$path = clim_check
}


# #add runs ids from previous forecast to settings object to be passed to build X
# run_id <- list()
# for (k in 1:length(run$id)) {
#   run_id[[k]] = as.character(run$id[k])
# }
# names(run_id) = sprintf("id%s",seq(1:length(run$id))) #rename list
# settings$runs$id = run_id

# #add run ids from previous sda to settings object to be passed to build X
# run_id <- list()
# for (k in 1:length(previous.ens)) {
#   run_id[[k]] = as.character(previous.ens[k])
# }
# names(run_id) = sprintf("id%s",seq(1:length(previous.ens))) #rename list
# settings$runs$id = run_id

#save restart object
save(restart, next.oldir, obs.mean, obs.cov, file = file.path(settings$outdir, "restart.Rdata"))
#run sda function
sda.enkf.multisite(settings = settings, 
                   obs.mean = obs.mean, 
                   obs.cov = obs.cov, 
                   Q = NULL, 
                   restart = restart, 
                   forceRun = TRUE, 
                   keepNC = TRUE, 
                   control = list(trace = TRUE,
                                  FF = FALSE,
                                  interactivePlot = FALSE,
                                  TimeseriesPlot = FALSE,
                                  BiasPlot = FALSE,
                                  plot.title = NULL,
                                  facet.plots = FALSE,
                                  debug = FALSE,
                                  pause = FALSE,
                                  Profiling = FALSE,
                                  OutlierDetection=FALSE))





}
