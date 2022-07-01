# ----------------------------------------------------------------------
#------------------------------------------ Load required libraries-----
# ----------------------------------------------------------------------
library("PEcAn.all")
library("PEcAn.utils")
library("PEcAn.data.remote")
library("PEcAn.assim.sequential")
library("RCurl")
library("REddyProc")
library("tidyverse")
library("furrr")
library("R.utils")
library("dynutils")
library('nimble')
plan(multisession)


# ----------------------------------------------------------------------------------------------
#------------------------------------------Prepared SDA Settings -----
# ----------------------------------------------------------------------------------------------

forecastPath <- "/projectnb/dietzelab/ahelgeso/Site_Outputs/Harvard/FluxPaper/"
outputPath <- "/projectnb/dietzelab/ahelgeso/SDA/HF_SDA_Output/"
nodata <- FALSE #use this to run SDA with no data
restart <- list()
#days.obs <- 3 #how many of observed data *BY HOURS* to include -- not including today
setwd(outputPath)
options(warn=-1)

#------------------------------------------------------------------------------------------------
#------------------------------------------ Preparing the pecan xml -----------------------------
#------------------------------------------------------------------------------------------------

#reading xml
settings <- read.settings("/projectnb/dietzelab/ahelgeso/pecan/modules/assim.sequential/inst/WillowCreek/testingMulti_HF.xml")

#connecting to DB
con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)
on.exit(db.close(con))
#Find last SDA Run to get new start date
sda.start <- NA
all.previous.sims <- list.dirs(outputPath, recursive = F)

#to manually change start date 
sda.start <- as.Date("2021-07-28")
sda.end <- sda.start + lubridate::days(1)

# Finding the right end and start date
met.start <- sda.start
met.end <- met.start + lubridate::days(35)

# --------------------------------------------------------------------------------------------------
#---------------------------------------------- LAI DATA -------------------------------------
# --------------------------------------------------------------------------------------------------
site_info <- list(
  site_id = settings$run$site$id,
  site_name = settings$run$site$name,
  lat = settings$run$site$lat,
  lon = settings$run$site$lon,
  time_zone = "UTC")

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
    lai = NA
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
    lai_sd = NA
    PEcAn.logger::logger.warn(paste0("MODIS standard deviation Data not available for these dates, initialzing NA"))
  }

#build obs mean/cov matrix for LAI
  obs.mean <- data.frame(date = lai$calendar_date, site_id = lai$site_id, lai = lai$data)
  obs.mean$date = as.character(obs.mean$date, stringsAsFactors = FALSE)
  obs.mean <- split(obs.mean, obs.mean$date)
  
  obs.cov <- data.frame(date = lai_sd$calendar_date, site_id = lai_sd$site_id, lai = lai_sd$data)
  obs.cov$date = as.character(obs.cov$date, stringsAsFactors = FALSE)
  obs.cov <- split(obs.cov, obs.cov$date)


#-----------------------------------------------------------------------------------------------
#------------------------------------------ Fixing the settings --------------------------------
#-----------------------------------------------------------------------------------------------
#Using the found dates to run - this will help to download mets
settings$run$site$met.start <- as.character(met.start)
settings$run$site$met.end <- as.character(met.end)
#info
settings$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"), " +0000")

# --------------------------------------------------------------------------------------------------
#---------------------------------------------- PEcAn Workflow -------------------------------------
# --------------------------------------------------------------------------------------------------
#Update/fix/check settings. Will only run the first time it's called, unless force=TRUE
settings <- PEcAn.settings::prepare.settings(settings, force=TRUE)
setwd(settings$outdir)
#Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")
# start from scratch if no continue is passed in
statusFile <- file.path(settings$outdir, "STATUS")
if (length(which(commandArgs() == "--continue")) == 0 && file.exists(statusFile)) {
  file.remove(statusFile)
}


#manually add in clim files 
con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)

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

#query database for previous forecast run (i.e. t=0)
query.run <- paste0("SELECT * FROM runs WHERE site_id =", site_info$site_id)
run <- PEcAn.DB::db.query(query.run, con)
#filter for sda.start
run <- dplyr::filter(run, start_time == sda.start)
daydiff <- difftime(Sys.time(), run$created_at, units = "days")
runday <- which(min(daydiff) == daydiff)
startday <- run$created_at[runday]
run <- dplyr::filter(run, as.Date(created_at) == as.Date(startday))
#add filter for model
query.ens <- paste0("SELECT * FROM ensembles WHERE id =", run$ensemble_id)
ens <- PEcAn.DB::db.query(query.ens, con)
#now that we have the workflow id for forecast run we can close connection to BETY
PEcAn.DB::db.close(con)
#list files in output folder
restart$filepath <- paste0(forecastPath, "PEcAn_", ens$workflow_id, "/")
restart$start.cut <- lubridate::as_datetime(obs.mean$`2021-06-02`$date)
restart$start.cut <- format(restart$start.cut, "%Y-%m-%d %H:%M:%S", tz = "EST")
restart$runids <- run$id

# Setting dates in assimilation tags - This will help with preprocess split in SDA code
settings$state.data.assimilation$start.date <-as.character(sda.start)
settings$state.data.assimilation$end.date <-as.character(sda.end)

if ('state.data.assimilation' %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    PEcAn.assim.sequential::sda.enkf.multisite(
      settings, 
      restart=restart,
      Q=0,
      obs.mean = obs.mean,
      obs.cov = obs.cov,
      control = list(
        trace = TRUE,
        interactivePlot =FALSE,
        TimeseriesPlot =TRUE,
        BiasPlot =FALSE,
        debug = FALSE,
        pause=FALSE
      )
    )
    
    PEcAn.utils::status.end()
  }
}





