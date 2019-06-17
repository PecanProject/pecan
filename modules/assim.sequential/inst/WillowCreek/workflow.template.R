# ----------------------------------------------------------------------
#------------------------------------------ Load required libraries-----
# ----------------------------------------------------------------------
library(PEcAn.all)
library(PEcAn.utils)
library(RCurl)
library(REddyProc)
library(tidyverse)
library(furrr)
plan(multiprocess)
# ----------------------------------------------------------------------------------------------
#------------------------------------------ That's all we need xml path and the out folder -----
# ----------------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (is.na(args[1])){
  xmlTempName <-"gefs.sipnet.template.xml"
} else {
  xmlTempName = args[1]
}

if (is.na(args[2])){
  outputPath <- "/fs/data3/kzarada/ouput"
} else {
  outputPath = args[2]
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
settings <- read.settings(system.file("WillowCreek",
                                      xmlTempName,
                                      package ="PEcAn.assim.sequential" ))

#settings <- read.settings('/home/hamzed/R/library/PEcAn.assim.sequential/WillowCreek/gefs.sipnet.template.xml')
#connecting to DB
con <-try(PEcAn.DB::db.open(settings$database$bety), silent = TRUE)

#------------------------------------------------------------------------------------------------
#------------------------------------------ Preparing the pecan xml -----------------------------
#------------------------------------------------------------------------------------------------
#--------------------------- Finding old sims
all.previous.sims <-list.dirs(outputPath, recursive = F)

if (length(all.previous.sims)>0 & !inherits(con, "try-error")){
  # Looking through all the old simulations and find the most recent
  sda.start <-all.previous.sims %>%
    map_chr( ~ strsplit(.x, "_")[[1]][2]) %>%
    map_dfr(~db.query(query = paste("SELECT started_at FROM workflows WHERE id =", .x), con = con)) %>%
    mutate(started_at=as.Date(started_at)) %>% 
    arrange(desc(started_at)) %>%
    head(1) %>%
    pull
  # if there was no older sims
  if (is.na(sda.start))  sda.start <-Sys.Date()-14
}else{
  sda.start <-Sys.Date()-14
}

sda.end <- Sys.Date()

#-----------------------------------------------------------------------------------------------
#------------------------------------------ Download met and flux ------------------------------
#-----------------------------------------------------------------------------------------------
#Fluxes
if(!exists('prep.data'))
prep.data <- prep.data.assim(sda.start-90, sda.end, numvals = 100, vars = c("NEE", "LE"), data.len = 168) 
obs.raw <-prep.data$rawobs
prep.data<-prep.data$obs
# This line is what makes the SDA to run daily
prep.data<-prep.data %>%
  discard(~lubridate::hour(.x$Date)!=0)


# Finding the right end and start date
met.start <- obs.raw$Date%>% head(1) %>% lubridate::floor_date(unit = "day")
met.end <- obs.raw$Date %>% tail(1) %>% lubridate::ceiling_date(unit = "day")

#Downloading met
#met.raw <- download_US_WCr_met(met.start, met.end)
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
ggsave(file.path(settings$outdir,"Obs_plot.pdf"), ploting_fluxes(obs.raw) , width = 18, height = 10)

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
# Changing LE to Qle whih what sipnet understands
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

obs.mean <-prep.data %>% map('means') %>% setNames(names(prep.data))
obs.cov <- prep.data %>% map('covs') %>% setNames(names(prep.data))

if (TRUE) {
  obs.mean <- obs.mean %>% map(function(x)return(NA))
  obs.cov <- obs.cov %>% map(function(x)return(NA))
}
# --------------------------------------------------------------------------------------------------
#--------------------------------- Run state data assimilation -------------------------------------
# --------------------------------------------------------------------------------------------------

unlink(c('run','out','SDA'), recursive = T)

if ('state.data.assimilation' %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
  PEcAn.assim.sequential::sda.enkf(
      settings,
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
