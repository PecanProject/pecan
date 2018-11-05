# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------
library(PEcAn.all)
library(PEcAn.utils)
library(RCurl)
#------------------------------------------
setwd("/fs/data3/hamzed/pecan/modules/assim.sequential/inst/WillowCreek")
source('Utils.R')
source('Download_US_Wcr.R')
source('prep.data.assim.R')
outputPath <- "/fs/data3/hamzed/Projects/WillowCreek"
xmlPath <-"/fs/data3/hamzed/pecan/modules/assim.sequential/inst/WillowCreek/gefs.sipnet.template.xml"
#------------------------------------------------------ Preparing the pecan xml
# Open and read in settings file for PEcAn run.
args <- commandArgs(trailingOnly = TRUE)
if (is.na(args[1])){
  settings <- PEcAn.settings::read.settings(xmlPath) 
} else {
  settings.file = args[1]
  settings <- PEcAn.settings::read.settings(settings.file)
}

#---------- Data Prep for flux data 




#---------- Finding the start and end date - because download met is going to use it
if (exists("source_date")) {
  start_date <- round.to.six.hours(source_date)
} else {
  start_date <- round.to.six.hours()
}
end_date <- start_date + lubridate::days(16)
# Using the found dates to run
settings$run$start.date <- as.character(start_date)
settings$run$end.date <- as.character(end_date)
#info
settings$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"), " +0000")
# and ensemble dates
settings$ensemble$start.year <- format(start_date, "%Y")
settings$ensemble$end.year <- as.character(end_date, "%Y")
#and SDA dates 
settings$state.data.assimilation$state.date <- as.character(start_date)
settings$state.data.assimilation$end.date <- as.character(Sys.Date())
#-- Setting the out dir
settings$outdir <- file.path(outputPath, Sys.time() %>% as.numeric())


#--------------------------- Preparing OBS  data
start.Date.obs <- Sys.Date() 
obs.raw <- download_US_WCr_flux(start.Date.obs-1,start.Date.obs)

#--------------------------- Calling in prepped data 





#--------- Making a plot
obs.plot <- obs.raw %>%
            tidyr::gather(Param, Value, -c(Date)) %>%
            filter(!(Param %in% c("Fjday", "U"))) %>%
            ggplot(aes(Date, Value)) +
            geom_line(aes(color = Param), lwd = 1) +
            geom_point(aes(color = Param), size = 3) +
            facet_wrap( ~ Param, scales = "free",ncol = 2) +
            scale_color_brewer(palette = "Set1") +
            theme_minimal(base_size = 15) +
            labs(y = "") +
            theme(legend.position = "none")
#obs.plot
# Make sure you have the premission - chmod is right
dir.create(settings$outdir)
ggsave(file.path(settings$outdir,"Obs_plot.png"), obs.plot , width = 18, height = 9)


# ----------------------------------------------------------------------
# PEcAn Workflow
# ----------------------------------------------------------------------

# Check for additional modules that will require adding settings
if("benchmarking" %in% names(settings)){
  library(PEcAn.benchmark)
  settings <- papply(settings, read_settings_BRR)
}
if("sitegroup" %in% names(settings)){
  if(is.null(settings$sitegroup$nSite)){
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings, sitegroupId = settings$sitegroup$id)
  } else {
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings, sitegroupId = settings$sitegroup$id,nSite = settings$sitegroup$nSite)
  }
  settings$sitegroup <- NULL ## zero out so don't expand a second time if re-reading
}
# Update/fix/check settings. Will only run the first time it's called, unless force=TRUE
settings <- PEcAn.settings::prepare.settings(settings, force=FALSE)
# Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")
# start from scratch if no continue is passed in
statusFile <- file.path(settings$outdir, "STATUS")
if (length(which(commandArgs() == "--continue")) == 0 && file.exists(statusFile)) {
  file.remove(statusFile)
}
# Do conversions
settings <- PEcAn.utils::do_conversions(settings)

# Query the trait database for data and priors
if (PEcAn.utils::status.check("TRAIT") == 0){
  PEcAn.utils::status.start("TRAIT")
  settings <- PEcAn.workflow::runModule.get.trait.data(settings)
  PEcAn.settings::write.settings(settings, outputfile='pecan.TRAIT.xml')
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, 'pecan.TRAIT.xml'))) {
  settings <- PEcAn.settings::read.settings(file.path(settings$outdir, 'pecan.TRAIT.xml'))
}


# Run the PEcAn meta.analysis
if(!is.null(settings$meta.analysis)) {
  if (PEcAn.utils::status.check("META") == 0){
    PEcAn.utils::status.start("META")
    PEcAn.MA::runModule.run.meta.analysis(settings)
    PEcAn.utils::status.end()
  }
}


# Write model specific configs
if (PEcAn.utils::status.check("CONFIG") == 0){
  PEcAn.utils::status.start("CONFIG")
  settings <- runModule.run.write.configs(settings)
  PEcAn.settings::write.settings(settings, outputfile='pecan.CONFIGS.xml')
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, 'pecan.CONFIGS.xml'))) {
  settings <- PEcAn.settings::read.settings(file.path(settings$outdir, 'pecan.CONFIGS.xml'))
}
print("---------- Wrtting Configs Completed ----------")

if ((length(which(commandArgs() == "--advanced")) != 0) && (PEcAn.utils::status.check("ADVANCED") == 0)) {
  PEcAn.utils::status.start("ADVANCED")
  q();
}

# Start ecosystem model runs
if (PEcAn.utils::status.check("MODEL") == 0) {
  PEcAn.utils::status.start("MODEL")
  PEcAn.remote::runModule.start.model.runs(settings,stop.on.error=FALSE)
  PEcAn.utils::status.end()
}
print("---------- Model runs Completed ----------")
# Get results of model runs
if (PEcAn.utils::status.check("OUTPUT") == 0) {
  PEcAn.utils::status.start("OUTPUT")
  runModule.get.results(settings)
  PEcAn.utils::status.end()
}

# Run ensemble analysis on model output. 
if ('ensemble' %in% names(settings) & PEcAn.utils::status.check("ENSEMBLE") == 0) {
  PEcAn.utils::status.start("ENSEMBLE")
  runModule.run.ensemble.analysis(settings, TRUE)    
  PEcAn.utils::status.end()
}
print("---------- Ensemble Completed ----------")
# Run sensitivity analysis and variance decomposition on model output
if ('sensitivity.analysis' %in% names(settings) & PEcAn.utils::status.check("SENSITIVITY") == 0) {
  PEcAn.utils::status.start("SENSITIVITY")
  runModule.run.sensitivity.analysis(settings)
  PEcAn.utils::status.end()
}

# Run parameter data assimilation
if ('assim.batch' %in% names(settings)) {
  if (PEcAn.utils::status.check("PDA") == 0) {
    PEcAn.utils::status.start("PDA")
    settings <- PEcAn.assim.batch::runModule.assim.batch(settings)
    PEcAn.utils::status.end()
  }
}

# Run state data assimilation
if ('state.data.assimilation' %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    settings <- PEcAn.assim.sequential::sda.enkf(settings,obs.list$obs.mean,obs.list$obs.cov)
    PEcAn.utils::status.end()
  }
}

# Run benchmarking
if("benchmarking" %in% names(settings)){
  PEcAn.utils::status.start("BENCHMARKING")
  results <- papply(settings, function(x) calc_benchmark(x, bety))
  PEcAn.utils::status.end()
}

# Pecan workflow complete
if (PEcAn.utils::status.check("FINISHED") == 0) {
  PEcAn.utils::status.start("FINISHED")
  PEcAn.remote::kill.tunnel(settings)
  db.query(paste("UPDATE workflows SET finished_at=NOW() WHERE id=", settings$workflow$id, "AND finished_at IS NULL"), params=settings$database$bety)
  
  # Send email if configured
  if (!is.null(settings$email) && !is.null(settings$email$to) && (settings$email$to != "")) {
    sendmail(settings$email$from, settings$email$to,
             paste0("Workflow has finished executing at ", base::date()),
             paste0("You can find the results on ", settings$email$url))
  }
  PEcAn.utils::status.end()
}

db.print.connections()
print("---------- PEcAn Workflow Complete ----------")
