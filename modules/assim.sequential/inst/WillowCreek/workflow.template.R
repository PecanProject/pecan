# ----------------------------------------------------------------------
#------------------------------------------ Load required libraries-----
# ----------------------------------------------------------------------
library(PEcAn.all)
library(PEcAn.utils)
library(RCurl)
library(REddyProc)
library(tidyverse)
library(furrr)
library(R.utils)
plan(multiprocess)
# ----------------------------------------------------------------------------------------------
#------------------------------------------ That's all we need xml path and the out folder -----
# ----------------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (is.na(args[1])){
  outputPath <- "/fs/data3/kzarada/ouput"
} else {
  outputPath <- args[1]
}

if (is.na(args[2])){
  nodata <- FALSE
} else {
  nodata <-as.logical(args[2])
}

if (is.na(args[3])){
  xmlTempName <-"gefs.sipnet.template.xml"
} else {
  xmlTempName <- args[3]
}
setwd(outputPath)
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
#reading xml
settings <- read.settings("/fs/data3/kzarada/pecan/modules/assim.sequential/inst/WillowCreek/gefs.sipnet.template.xml")

#connecting to DB
con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)

#------------------------------------------------------------------------------------------------
#------------------------------------------ Preparing the pecan xml -----------------------------
#------------------------------------------------------------------------------------------------
#--------------------------- Finding old sims
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
      map_chr( ~ strsplit(.x, "_")[[1]][2]) %>%
      map_dfr(~ db.query(
        query = paste("SELECT started_at FROM workflows WHERE id =", .x),
        con = con
      ) %>% 
        mutate(ID=.x)) %>%
      mutate(started_at = as.Date(started_at)) %>%
      arrange(desc(started_at)) %>%
      head(1)
    # pulling the date and the path to the last SDA
    restart.path <-grep(last.sim$ID, names(all.previous.sims), value = T)
    sda.start <- last.sim$started_at
  },
  error = function(e) {
    restart.path <- NULL
    sda.start <- Sys.Date() - 14
    PEcAn.logger::logger.warn(paste0("There was a problem with finding the last successfull SDA.",conditionMessage(e)))
  })
  
  # if there was no older sims
  if (is.na(sda.start))
    sda.start <- Sys.Date() - 14
}

sda.end <- Sys.Date()

#-----------------------------------------------------------------------------------------------
#------------------------------------------ Download met and flux ------------------------------
#-----------------------------------------------------------------------------------------------
#Fluxes
if(!exists('prep.data'))
  prep.data <- prep.data.assim(
    sda.start - 90,# it needs at least 90 days for gap filling 
    sda.end,
    numvals = 100,
    vars = c("NEE", "LE"),
    data.len = 168 # This is 7 days
  ) 
obs.raw <-prep.data$rawobs
prep.data<-prep.data$obs
# This line is what makes the SDA to run daily  ***** IMPORTANT CODE OVER HERE
prep.data<-prep.data %>%
  discard(~lubridate::hour(.x$Date)!=0)

# Finding the right end and start date
met.start <- obs.raw$Date%>% head(1) %>% lubridate::floor_date(unit = "day")
met.end <- obs.raw$Date %>% tail(1) %>% lubridate::ceiling_date(unit = "day")
#-----------------------------------------------------------------------------------------------
#------------------------------------------ Fixing the settings --------------------------------
#-----------------------------------------------------------------------------------------------
#Using the found dates to run - this will help to download mets
settings$run$start.date <- as.character(met.start)
settings$run$end.date <- as.character(met.end)
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
settings$state.data.assimilation$start.date <-as.character(met.start)
settings$state.data.assimilation$end.date <-as.character(met.end - lubridate::hms("06:00:00"))
# Changing LE to Qle which is what sipnet expects
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

# if there is infinte value then take it out
prep.data<-prep.data %>% 
  map(function(day.data){
    #cheking the mean
    nan.mean <- which(is.infinite(day.data$means) | is.nan(day.data$means) | is.na(day.data$means))
    if ( length(nan.mean)>0 ) {
      
      day.data$means <- day.data$means[-nan.mean]
      day.data$covs <- day.data$covs[-nan.mean, -nan.mean] %>%
        as.matrix() %>%
        `colnames<-`(c(colnames(day.data$covs)[-nan.mean]))
    }
    day.data
  })

obs.mean <- prep.data %>% map('means') %>% setNames(names(prep.data))
obs.cov <- prep.data %>% map('covs') %>% setNames(names(prep.data))

if (nodata) {
  obs.mean <- obs.mean %>% map(function(x)
    return(NA))
  obs.cov <- obs.cov %>% map(function(x)
    return(NA))
}

# --------------------------------------------------------------------------------------------------
#--------------------------------- Restart -------------------------------------
# --------------------------------------------------------------------------------------------------

#@Hamze - should we add a if statement here for the times that we don't want to copy the path?

  if(!dir.exists("SDA")) dir.create("SDA",showWarnings = F)
  
  file.copy(from= file.path(restart.path, "SDA", "sda.output.Rdata"),
            to = file.path(settings$outdir, "SDA", "sda.output.Rdata"))
  
  file.copy(from= file.path(restart.path, "SDA", "outconfig.Rdata"),
            to = file.path(settings$outdir, "SDA", "outconfig.Rdata"))
 
#Update the SDA Output to just have last time step 
  load(file.path(restart.path, "SDA", "sda.output.Rdata"))
  
  ANALYSIS1 = list()
  FORECAST1 = list()
  enkf.params1 = list()
  ANALYSIS1[[1]]= ANALYSIS[[length(ANALYSIS)]]
  FORECAST1[[1]] = FORECAST[[length(FORECAST)]]
  enkf.params1[[1]] = enkf.params[[length(enkf.params)]]
  t = 1 
  ANALYSIS = ANALYSIS1
  FORECAST = FORECAST1
  enfk.params = enkf.params1

  save(list = c("ANALYSIS", "FORECAST", "enkf.params", "t", "ensemble.samples", "inputs", "new.params", "new.state", "site.locs", "Viz.output", "X", "ensemble.id", "run.id"), file = file.path(settings$outdir, "SDA", "sda.output.Rdata"))  

#copy over run and out folders 
  
  if(!dir.exists("run")) dir.create("run",showWarnings = F)
  copyDirectory(from = file.path(restart.path, "run/"), 
                to = file.path(settings$outdir, "run/"))
  if(!dir.exists("out")) dir.create("out",showWarnings = F)
  copyDirectory(from = file.path(restart.path, "out/"), 
                to = file.path(settings$outdir, "out/"))
  
# --------------------------------------------------------------------------------------------------
#--------------------------------- Run state data assimilation -------------------------------------
# --------------------------------------------------------------------------------------------------

#unlink(c('run','out', "SDA"), recursive = T)

if ('state.data.assimilation' %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    PEcAn.assim.sequential::sda.enkf(
      settings,
      restart=TRUE,
      Q=0,
      obs.mean = obs.mean,
      obs.cov = obs.cov,
      control = list(
        trace = TRUE,
        interactivePlot =FALSE,
        TimeseriesPlot =FALSE,
        BiasPlot =FALSE,
        debug =FALSE,
        pause=FALSE
      )
    )
    PEcAn.utils::status.end()
  }
}

  