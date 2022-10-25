# ----------------------------------------------------------------------
#------------------------------------------ Load required libraries-----
# ----------------------------------------------------------------------
library("PEcAn.all")
library("PEcAn.utils")
library("RCurl")
library("REddyProc")
library("tidyverse")
library("furrr")
library("R.utils")
library("dynutils")
plan(multisession)


# ----------------------------------------------------------------------------------------------
#------------------------------------------ That's all we need xml path and the out folder -----
# ----------------------------------------------------------------------------------------------

outputPath <- "/projectnb/dietzelab/ahelgeso/SDA/Wcr_SDA_Output/NoData/"
nodata <- TRUE
restart <- FALSE
days.obs <- 1  #how many of observed data to include -- not including today
setwd(outputPath)

c(
  'Utils.R',
  'download_WCr.R',
  "gapfill_WCr.R",
  'prep.data.assim.R'
) %>% walk( ~ source(
  system.file("WillowCreek",
              .x,
              package = "PEcAnAssimSequential")
))


#------------------------------------------------------------------------------------------------
#------------------------------------------ Preparing the pecan xml -----------------------------
#------------------------------------------------------------------------------------------------
#--------------------------- Finding old sims


setwd("/projectnb/dietzelab/ahelgeso/SDA/Wcr_SDA_Output/NoData/")

#reading xml
settings <- read.settings("/projectnb/dietzelab/ahelgeso/pecan/modules/assim.sequential/inst/WillowCreek/nodata.xml")

#connecting to DB
con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)



# all.previous.sims <- list.dirs(outputPath, recursive = F)
# if (length(all.previous.sims) > 0 & !inherits(con, "try-error")) {
#   
#   tryCatch({
#     # Looking through all the old simulations and find the most recent
#     all.previous.sims <- all.previous.sims %>%
#       map(~ list.files(path = file.path(.x, "SDA"))) %>%
#       setNames(all.previous.sims) %>%
#       discard( ~ !"sda.output.Rdata" %in% .x) # I'm throwing out the ones that they did not have a SDA output
#     
#     last.sim <-
#       names(all.previous.sims) %>%
#       map_chr( ~ strsplit(.x, "_")[[1]][5]) %>%
#       map_dfr(~ db.query(
#         query = paste("SELECT * FROM workflows WHERE id =", .x),
#         con = con
#       ) %>%
#         mutate(ID=.x)) %>%
#       mutate(start_date = as.Date(start_date)) %>%
#       arrange(desc(start_date), desc(ID)) %>%
#       head(1)
#     # pulling the date and the path to the last SDA
#     restart.path <-grep(last.sim$ID, names(all.previous.sims), value = T)
#     sda.start <- last.sim$start_date + lubridate::days(1)
#   },
#   error = function(e) {
#     restart.path <- NULL
#     sda.start <- Sys.Date() - 1
#     PEcAn.logger::logger.warn(paste0("There was a problem with finding the last successfull SDA.",conditionMessage(e)))
#   })
#   
#   # if there was no older sims
#   if (is.na(sda.start))
#     sda.start <- Sys.Date() - 9
# }
sda.start <- Sys.Date()
sda.end <- sda.start + lubridate::days(3)
#-----------------------------------------------------------------------------------------------
#------------------------------------------ Download met and flux ------------------------------
#-----------------------------------------------------------------------------------------------


# Finding the right end and start date
met.start <- sda.start - lubridate::days(2)
met.end <- met.start + lubridate::days(16)


#pad Observed Data to match met data 

date <-
  seq(
    from = lubridate::with_tz(as.POSIXct(sda.start, format = "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    to = lubridate::with_tz(as.POSIXct(sda.end, format = "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
    by = "1 hour"
  )

pad.prep <- as.data.frame(cbind(Date = as.character(date), means = rep("NA", length(date)), covs = rep("NA", length(date)))) %>%
  dynutils::tibble_as_list()

names(pad.prep) <-date


prep.data = pad.prep



obs.mean <- prep.data %>%
  purrr::map('means') %>% 
  setNames(names(prep.data))
obs.cov <- prep.data %>% purrr::map('covs') %>% setNames(names(prep.data))

if (nodata) {
  obs.mean <- obs.mean %>% purrr::map(function(x)
    return(NA))
  obs.cov <- obs.cov %>% purrr::map(function(x)
    return(NA))
}


#-----------------------------------------------------------------------------------------------
#------------------------------------------ Fixing the settings --------------------------------
#-----------------------------------------------------------------------------------------------
#unlink existing IC files
sapply(paste0("/projectnb/dietzelabe/pecan.data/dbfiles/IC_site_0-676_", 1:100, ".nc"), unlink)
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


#Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")
# start from scratch if no continue is passed in
statusFile <- file.path(settings$outdir, "STATUS")
if (length(which(commandArgs() == "--continue")) == 0 && file.exists(statusFile)) {
  file.remove(statusFile)
}
# Do conversions
settings <- PEcAn.workflow::do_conversions(settings, T, T, T)

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

#- lubridate::hms("06:00:00")

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
  save(list = c("ANALYSIS",  "enkf.params", "ensemble.id", "ensemble.samples", 'inputs', 'new.params', 'new.state', 'run.id', 'site.locs', 't', 'Viz.output', 'X'),
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
settings$host$user <- "ahelgeso"
settings$host$folder <- "/projectnb/dietzelab/ahelgeso/SDA/Wcr_SDA_Output/"
settings$host$job.sh <- "module load R/4.1.2" 
settings$host$qsub <- 'qsub -l h_rt=24:00:00 -V -N @NAME@ -o @STDOUT@ -e @STDERR@'
settings$host$qsub.jobid <- 'Your job ([0-9]+) .*'
settings$host$qstat <- 'qstat -j @JOBID@ || echo DONE'
settings$host$tunnel <- "/projectnb/dietzelab/ahelgeso/tunnel"
settings$model$binary = "/usr2/postdoc/istfer/SIPNET/1023/sipnet"


unlink(c('run','out'), recursive = T)

#debugonce(PEcAnAssimSequential::sda.enkf)
if ('state.data.assimilation' %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    PEcAnAssimSequential::sda.enkf(
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


