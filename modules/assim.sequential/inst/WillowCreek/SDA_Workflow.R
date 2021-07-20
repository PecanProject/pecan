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

outputPath <- "/projectnb/dietzelab/kzarada/US_WCr_SDA_output/"
nodata <- FALSE #use this to run SDA with no data
restart <- FALSE#flag to start from previous run or not
days.obs <- 3 #how many of observed data *BY HOURS* to include -- not including today
setwd(outputPath)
options(warn=-1)


#------------------------------------------------------------------------------------------------
#------------------------------------------ sourcing the required tools -------------------------
#------------------------------------------------------------------------------------------------
c(
  'Utils.R',
  'download_WCr.R',
  "gapfill_WCr.R",
  'prep.data.assim.R'
) %>% walk( ~ source(
  system.file("WillowCreek",
              .x,
              package = "PEcAn.assim.sequential")
))

#------------------------------------------------------------------------------------------------
#------------------------------------------ Preparing the pecan xml -----------------------------
#------------------------------------------------------------------------------------------------

#reading xml
settings <- read.settings("/fs/data3/kzarada/pecan/modules/assim.sequential/inst/WillowCreek/testing.xml")

#connecting to DB
con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)

#Find last SDA Run to get new start date 
all.previous.sims <- list.dirs(outputPath, recursive = F)
if (length(all.previous.sims) > 0 & !inherits(con, "try-error")) {
  
  tryCatch({
    # Looking through all the old simulations and find the most recent
    all.previous.sims <- all.previous.sims %>%
      map(~ list.files(path = file.path(.x, "SDA"))) %>%
      setNames(all.previous.sims) %>%
      discard( ~ !"sda.output.Rdata" %in% .x) # I'm throwing out the ones that they did not have a SDA output
    
    last.sim <-
      names(all.previous.sims) %>%
      map_chr( ~ strsplit(.x, "_")[[1]][5]) %>%
      map_dfr(~ db.query(
        query = paste("SELECT * FROM workflows WHERE id =", .x),
        con = con
      ) %>%
        mutate(ID=.x)) %>%
      mutate(start_date = as.Date(start_date)) %>%
      arrange(desc(start_date), desc(ID)) %>%
      head(1)
    # pulling the date and the path to the last SDA
    restart.path <-grep(last.sim$ID, names(all.previous.sims), value = T)
    sda.start <- last.sim$start_date+ lubridate::days(3)
  },
  error = function(e) {
    restart.path <- NULL
    sda.start <- Sys.Date() - 9
    PEcAn.logger::logger.warn(paste0("There was a problem with finding the last successfull SDA.",conditionMessage(e)))
  })
  
  # if there was no older sims
  if (is.na(sda.start))
    sda.start <- Sys.Date() - 9
}
#to manually change start date 
sda.start <- Sys.Date() - lubridate::days(15)
sda.end <- sda.start + lubridate::days(5)

# Finding the right end and start date
met.start <- sda.start - lubridate::days(2)
met.end <- met.start + lubridate::days(16)


#-----------------------------------------------------------------------------------------------
#------------------------------------------ Download met and flux ------------------------------
#-----------------------------------------------------------------------------------------------
#Fluxes
prep.data  <- prep.data.assim(
  sda.start - lubridate::days(90),# it needs at least 90 days for gap filling 
  sda.end,
  numvals = 100,
  vars = c("NEE", "LE"),
  data.len = days.obs, 
  sda.start)

obs.raw <-prep.data$rawobs
prep.data<-prep.data$obs

# if there is infinte value then take it out - here we want to remove any that just have one NA in the observed data 
prep.data <- prep.data %>% 
  map(function(day.data){
    #cheking the mean
    nan.mean <- which(is.infinite(day.data$means) | is.nan(day.data$means) | is.na(day.data$means))
    if ( length(nan.mean)>0 ) {
      
      day.data$means <- day.data$means[-nan.mean]
      day.data$covs <- day.data$covs[-nan.mean, -nan.mean] %>%
        as.matrix() %>%
        `colnames <-`(c(colnames(day.data$covs)[-nan.mean]))
    }
    day.data
  })


# Changing LE to Qle which is what SIPNET expects
prep.data <- prep.data %>%
  map(function(day.data) {
    names(day.data$means)[names(day.data$means) == "LE"] <- "Qle"
    dimnames(day.data$covs) <- dimnames(day.data$covs) %>%
      map(function(name) {
        name[name == "LE"] <- "Qle"
        name
      })
    
    day.data
  })


# --------------------------------------------------------------------------------------------------
#---------------------------------------------- LAI DATA -------------------------------------
# --------------------------------------------------------------------------------------------------

site_info <- list(
  site_id = 676,
  site_name = "Willow Creek",
  lat = 45.805925,
  lon = -90.07961,
  time_zone = "UTC")

tryCatch({
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
  lai <- lai %>% filter(qc == "000")}, 
  error = function(e) {
    lai <- NULL
    PEcAn.logger::logger.warn(paste0("MODIS Data not available for these dates",conditionMessage(e)))
  }
)
if(!exists('lai')){lai = NULL}


tryCatch({
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
  lai_sd <- lai_sd %>% filter(qc == "000")}, 
  error = function(e) {
    lai_sd <- NULL
    PEcAn.logger::logger.warn(paste0("MODIS Data not available for these dates",conditionMessage(e)))
  }
)
if(!exists('lai_sd')){lai_sd = NULL}

###### Pad Observed Data to forecast ############# 

date <-
  seq(
    from = lubridate::force_tz(as.POSIXct(last(names(prep.data)), format = "%Y-%m-%d %H:%M:%S"), tz = "UTC") + lubridate::hours(1),
    to = lubridate::with_tz(as.POSIXct(first(sda.end) + lubridate::days(1), format = "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    by = "1 hour"
  )

pad.prep <- obs.raw %>%
  tidyr::complete(Date = date) %>%
  filter(Date %in% date) %>% 
  mutate(means = NA, covs = NA) %>%
  dplyr::select(Date, means, covs) %>%
  dynutils::tibble_as_list() 

names(pad.prep) <-date


#Add in LAI info 

if(is.null(lai)){index <- rep(FALSE, length(names(prep.data)))}else{
  index <- as.Date(names(prep.data)) %in% as.Date(lai$calendar_date)
}


for(i in 1:length(index)){
  
  if(index[i]){
    lai.date <- which(as.Date(lai$calendar_date) %in% as.Date(names(prep.data)))
    LAI <- c(0,0)
    prep.data[[i]]$means <- c(prep.data[[i]]$means, lai$data[lai.date])
    prep.data[[i]]$covs <- rbind(cbind(prep.data[[i]]$covs, c(0, 0)), c(0,0, lai_sd$data))
    
    names(prep.data[[i]]$means) <- c("NEE", "Qle", "LAI")
    rownames(prep.data[[i]]$covs) <- c("NEE", "Qle", "LAI")
    colnames(prep.data[[i]]$covs) <- c("NEE", "Qle", "LAI")
    
  }
}

#add forecast pad to the obs data  
prep.data = c(prep.data, pad.prep)

#split into means and covs 

obs.mean <- prep.data %>%
  map('means') %>% 
  setNames(names(prep.data))
obs.cov <- prep.data %>% map('covs') %>% setNames(names(prep.data))




#-----------------------------------------------------------------------------------------------
#------------------------------------------ Fixing the settings --------------------------------
#-----------------------------------------------------------------------------------------------
#unlink existing IC files
sapply(paste0("/projectnb/dietzelab/pecan.data/dbfiles/BADM_site_0-676/IC_site_0-676_", 1:100, ".nc"), unlink)
#Using the found dates to run - this will help to download mets
settings$run$start.date <- as.character(met.start)
settings$run$end.date <- as.character(met.end)
settings$run$site$met.start <- as.character(met.start)
settings$run$site$met.end <- as.character(met.end)
#info
settings$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"), " +0000")




# --------------------------------------------------------------------------------------------------
#---------------------------------------------- PEcAn Workflow -------------------------------------
# --------------------------------------------------------------------------------------------------
#Update/fix/check settings. Will only run the first time it's called, unless force=TRUE
settings <- PEcAn.settings::prepare.settings(settings, force=FALSE)
setwd(settings$outdir)
ggsave(
  file.path(settings$outdir, "Obs_plot.pdf"),
  ploting_fluxes(obs.raw) ,
  width = 16,
  height = 9
)

#Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")
# start from scratch if no continue is passed in
statusFile <- file.path(settings$outdir, "STATUS")
if (length(which(commandArgs() == "--continue")) == 0 && file.exists(statusFile)) {
  file.remove(statusFile)
}
# Do conversions

######### Check for input files and insert paths #############
  con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)

#checks for .nc files for NOAA GEFS
  input_check <- PEcAn.DB::dbfile.input.check(
    siteid=settings$run$site$id %>% as.character(),
    startdate = settings$run$start.date %>% as.Date,
    enddate = settings$run$end.date %>% as.Date,
    parentid = NA,
    mimetype="application/x-netcdf",
    formatname="CF Meteorology",
    con,
    hostname = PEcAn.remote::fqdn(),
    exact.dates = TRUE,
    pattern = "NOAA_GEFS",
    return.all=TRUE
  )
 
#new NOAA GEFS files were going through gapfilled so the parent
#files of the clim files were the CF_gapfilled nc files
  input_check_2 <- list()
  
  for(i in 1:length(input_check$id)){
  input_check_2[i] <- PEcAn.DB::dbfile.input.check(
    siteid=settings$run$site$id %>% as.character(),
    startdate = settings$run$start.date %>% as.Date,
    enddate = settings$run$end.date %>% as.Date,
    parentid = input_check$container_id[i],
    mimetype="application/x-netcdf",
    formatname="CF Meteorology",
    con,
    hostname = PEcAn.remote::fqdn(),
    exact.dates = TRUE,
    pattern = "NOAA_GEFS",
    return.all=TRUE
  )$container_id
  }

#this  if statement deals with the NOAA GEFS files that 
#were gapfilled, and allows for us to find the clim files of the ones that weren't
#the problem here is that when we get GEFS nc files -> clim files, 
#the GEFS nc files are the parent ids for finding the clim files 
#but with GEFS nc -> gapfilled nc -> clim, the gapfilled files are the parent ids for the clim files 
if(length(input_check_2)>1){
input_check_2 = unlist(input_check_2)

clim_check = list()
for(i in 1:length(input_check$id)){
  clim_check[[i]] = file.path(PEcAn.DB::dbfile.input.check(
  siteid=settings$run$site$id %>% as.character(),
  startdate = settings$run$start.date %>% as.Date,
  enddate = settings$run$end.date %>% as.Date,
  parentid = input_check_2[i],
  mimetype="text/csv",
  formatname="Sipnet.climna",
  con,
  hostname = PEcAn.remote::fqdn(),
  exact.dates = TRUE,
  pattern = "NOAA_GEFS",
  return.all=TRUE
  )$file_path, PEcAn.DB::dbfile.input.check(
    siteid=settings$run$site$id %>% as.character(),
    startdate = settings$run$start.date %>% as.Date,
    enddate = settings$run$end.date %>% as.Date,
    parentid = input_check_2[i],
    mimetype="text/csv",
    formatname="Sipnet.climna",
    con,
    hostname = PEcAn.remote::fqdn(),
    exact.dates = TRUE,
    pattern = "NOAA_GEFS",
    return.all=TRUE
  )$file_name)}}else{
    for(i in 1:length(input_check$id)){
      clim_check[[i]] = file.path(PEcAn.DB::dbfile.input.check(
        siteid=settings$run$site$id %>% as.character(),
        startdate = settings$run$start.date %>% as.Date,
        enddate = settings$run$end.date %>% as.Date,
        parentid = input_check$container_id[i],
        mimetype="text/csv",
        formatname="Sipnet.climna",
        con,
        hostname = PEcAn.remote::fqdn(),
        exact.dates = TRUE,
        pattern = "NOAA_GEFS",
        return.all=TRUE
      )$file_path, PEcAn.DB::dbfile.input.check(
        siteid=settings$run$site$id %>% as.character(),
        startdate = settings$run$start.date %>% as.Date,
        enddate = settings$run$end.date %>% as.Date,
        parentid = input_check$container_id[i],
        mimetype="text/csv",
        formatname="Sipnet.climna",
        con,
        hostname = PEcAn.remote::fqdn(),
        exact.dates = TRUE,
        pattern = "NOAA_GEFS",
        return.all=TRUE
      )$file_name)}
  }#end if/else look for making clim file paths 

  #If INPUTS already exists, add id and met path to settings file

  if(length(input_check$id) > 0){
    index_id = list()
    index_path = list()
    for(i in 1:length(input_check$id)){
      index_id[[i]] = as.character(dbfile.id(type = "Input",
                                             file = file.path(input_check$file_path,
                                                              input_check$file_name)[i], con = con))#get ids as list

    }#end i loop for making lists
    names(index_id) = sprintf("id%s",seq(1:length(input_check$id))) #rename list
    names(clim_check) = sprintf("path%s",seq(1:length(input_check$id)))

    settings$run$inputs$met$id = index_id
    settings$run$inputs$met$path = clim_check
  }

#still want to run this to get the IC files 
settings <- PEcAn.workflow::do_conversions(settings)          #end if loop for existing inputs  

 # if(is_empty(settings$run$inputs$met$path) & length(clim_check)>0){
#   settings$run$inputs$met$id = index_id
#   settings$run$inputs$met$path = clim_check
# }


# PEcAn.DB::db.close(con)
# Query the trait database for data and priors
if (PEcAn.utils::status.check("TRAIT") == 0) {
  PEcAn.utils::status.start("TRAIT")
  settings <- PEcAn.workflow::runModule.get.trait.data(settings)
  PEcAn.settings::write.settings(settings, outputfile = 'pecan.TRAIT.xml')
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, 'pecan.TRAIT.xml'))) {
  settings <-
    PEcAn.settings::read.settings(file.path(settings$outdir, 'pecan.TRAIT.xml'))
}
# Run the PEcAn meta.analysis
if (!is.null(settings$meta.analysis)) {
  if (PEcAn.utils::status.check("META") == 0) {
    PEcAn.utils::status.start("META")
    PEcAn.MA::runModule.run.meta.analysis(settings)
    PEcAn.utils::status.end()
  }
}
#sample from parameters used for both sensitivity analysis and Ens
get.parameter.samples(settings, ens.sample.method = settings$ensemble$samplingspace$parameters$method)
# Setting dates in assimilation tags - This will help with preprocess split in SDA code
settings$state.data.assimilation$start.date <-as.character(first(names(obs.mean)))
settings$state.data.assimilation$end.date <-as.character(last(names(obs.mean)))

if (nodata) {
  obs.mean <- obs.mean %>% map(function(x)
    return(NA))
  obs.cov <- obs.cov %>% map(function(x)
    return(NA))
}

# --------------------------------------------------------------------------------------------------
#--------------------------------- Restart -------------------------------------
# --------------------------------------------------------------------------------------------------

if(restart == TRUE){
  if(!dir.exists("SDA")) dir.create("SDA",showWarnings = F)
  
  #Update the SDA Output to just have last time step 
  temp<- new.env()
  load(file.path(restart.path, "SDA", "sda.output.Rdata"), envir = temp)
  temp <- as.list(temp)
  
  #we want ANALYSIS, FORECAST, and enkf.parms to match up with how many days obs data we have
  # +24 because it's hourly now and we want the next day as the start 
  if(length(temp$ANALYSIS) > 1){
    
    for(i in 1:days.obs + 1){ 
      temp$ANALYSIS[[i]] <- temp$ANALYSIS[[i + 24]]
    }
    for(i in rev((days.obs + 2):length(temp$ANALYSIS))){ 
      temp$ANALYSIS[[i]] <- NULL
    }
    
    
    for(i in 1:days.obs + 1){ 
      temp$FORECAST[[i]] <- temp$FORECAST[[i + 24]]
    }
    for(i in rev((days.obs + 2):length(temp$FORECAST))){ 
      temp$FORECAST[[i]] <- NULL
    }
    
    for(i in 1:days.obs + 1){ 
      temp$enkf.params[[i]] <- temp$enkf.params[[i + 24]]
    }
    for(i in rev((days.obs + 2):length(temp$enkf.params))){ 
      temp$enkf.params[[i]] <- NULL
    }    
    
  }
  temp$t = 1 
  
  #change inputs path to match sampling met paths 
  
  for(i in 1: length(temp$inputs$ids)){
    
    temp$inputs$samples[i] <- settings$run$inputs$met$path[temp$inputs$ids[i]]
    
  }
  
  temp1<- new.env()
  list2env(temp, envir = temp1)
  save(list = c("ANALYSIS", 'FORECAST', "enkf.params", "ensemble.id", "ensemble.samples", 'inputs', 'new.params', 'new.state', 'run.id', 'site.locs', 't', 'Viz.output', 'X'),
       envir = temp1, 
       file = file.path(settings$outdir, "SDA", "sda.output.Rdata"))  
  
  
  
  temp.out <- new.env()
  load(file.path(restart.path, "SDA", 'outconfig.Rdata'), envir = temp.out)
  temp.out <- as.list(temp.out)
  temp.out$outconfig$samples <- NULL
  
  temp.out1 <- new.env()
  list2env(temp.out, envir = temp.out1)
  save(list = c('outconfig'), 
       envir = temp.out1, 
       file = file.path(settings$outdir, "SDA", "outconfig.Rdata"))
  
  
  
  #copy over run and out folders 
  
  if(!dir.exists("run")) dir.create("run",showWarnings = F)
  
  files <- list.files(path = file.path(restart.path, "run/"), full.names = T, recursive = T, include.dirs = T, pattern = "sipnet.clim")
  readfiles <- list.files(path = file.path(restart.path, "run/"), full.names = T, recursive = T, include.dirs = T, pattern = "README.txt")
  
  newfiles <- gsub(pattern = restart.path, settings$outdir, files)
  readnewfiles <- gsub(pattern = restart.path, settings$outdir, readfiles)
  
  rundirs <- gsub(pattern = "/sipnet.clim", "", files)
  rundirs <- gsub(pattern = restart.path, settings$outdir, rundirs)
  for(i in 1 : length(rundirs)){
    dir.create(rundirs[i]) 
    file.copy(from = files[i], to = newfiles[i])
    file.copy(from = readfiles[i], to = readnewfiles[i])} 
  file.copy(from = paste0(restart.path, '/run/runs.txt'), to = paste0(settings$outdir,'/run/runs.txt' ))
  
  if(!dir.exists("out")) dir.create("out",showWarnings = F)
  
  files <- list.files(path = file.path(restart.path, "out/"), full.names = T, recursive = T, include.dirs = T, pattern = "sipnet.out")
  newfiles <- gsub(pattern = restart.path, settings$outdir, files)
  outdirs <- gsub(pattern = "/sipnet.out", "", files)
  outdirs <- gsub(pattern = restart.path, settings$outdir, outdirs)
  for(i in 1 : length(outdirs)){
    dir.create(outdirs[i]) 
    file.copy(from = files[i], to = newfiles[i])} 
  
} 
# --------------------------------------------------------------------------------------------------
#--------------------------------- Run state data assimilation -------------------------------------
# --------------------------------------------------------------------------------------------------

settings$host$name <- "geo.bu.edu"
settings$host$user <- 'kzarada'
settings$host$folder <- "/projectnb/dietzelab/kzarada/US_WCr_SDA_output"
settings$host$job.sh <- "module load udunits/2.2.26 R/3.5.1" 
settings$host$qsub <- 'qsub -l h_rt=24:00:00 -V -N @NAME@ -o @STDOUT@ -e @STDERR@'
settings$host$qsub.jobid <- 'Your job ([0-9]+) .*'
settings$host$qstat <- 'qstat -j @JOBID@ || echo DONE'
settings$host$tunnel <- '/tmp/tunnel'
settings$model$binary = "/usr2/postdoc/istfer/SIPNET/1023/sipnet"


source('/fs/data3/kzarada/pecan/modules/assim.sequential/R/Nimble_codes.R')


if(restart == FALSE) unlink(c('run','out','SDA'), recursive = T)
debugonce(PEcAn.assim.sequential::sda.enkf)

if ('state.data.assimilation' %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    PEcAn.assim.sequential::sda.enkf(
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





