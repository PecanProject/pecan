# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------
library(PEcAn.all)
library(PEcAn.utils)
library(RCurl)
library(REddyProc)
library(purrr)
#------------------------------------------
setwd("/fs/data3/kzarada/pecan/modules/assim.sequential/inst/WillowCreek")
source('Utils.R')
source('download_WCr_flux.R')
source('download_WCr_met.R')
source("gapfill_WCr.R")
source('prep.data.assim.R')
outputPath <- "/fs/data3/kzarada/Projects/WillowCreek"
xmlPath <-"/fs/data3/kzarada/pecan/modules/assim.sequential/inst/WillowCreek/gefs.sipnet.template.xml"
#------------------------------------------------------ Preparing the pecan xml
# Open and read in settings file for PEcAn run.
args <- commandArgs(trailingOnly = TRUE)
if (is.na(args[1])){
  settings <- PEcAn.settings::read.settings(xmlPath) 
} else {
  settings.file = args[1]
  settings <- PEcAn.settings::read.settings(settings.file)
}

#--------------------------- Calling in prepped data 
sda.end <- as.Date(Sys.Date())
sda.start <- as.Date(sda.end - lubridate::days(90))

prep.data <- prep.data.assim(sda.start, sda.end, numvals = 100, vars = c("NEE", "LE"), data.len = 72) 
#--------------------------- Preparing OBS  data
met.end <- prep.data[[length(prep.data)]]$Date
obs.raw <- download_US_WCr_flux(sda.start, met.end)  
met.raw <- download_US_WCr_met(sda.start, met.end)
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
#-- Setting the out dir
settings$outdir <- file.path(outputPath, Sys.time() %>% as.numeric())
#--------- Making a plot
obs.plot <- obs.raw %>%
            tidyr::gather(Param, Value, -c(date)) %>%
            filter(!(Param %in% c("FjDay", "U","Day","DoY","FC","FjFay","Hour","Month",
                                  "SC","Ustar","Year","H","Flag")),
                   Value!=-999) %>%
            ggplot(aes(date, Value)) +
            geom_line(aes(color = Param), lwd = 1) +
            geom_point(aes(color = Param), size = 3) +
            facet_wrap( ~ Param, scales = "free",ncol = 1) +
            scale_color_brewer(palette = "Set1") +
            theme_minimal(base_size = 15) +
            labs(y = "") +
            theme(legend.position = "none")

# Make sure you have the premission - chmod is right
dir.create(settings$outdir)
ggsave(file.path(settings$outdir,"Obs_plot.pdf"), obs.plot , width = 18, height = 10)


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
# if (PEcAn.utils::status.check("CONFIG") == 0){
#   PEcAn.utils::status.start("CONFIG")
#   settings <- runModule.run.write.configs(settings)
#   PEcAn.settings::write.settings(settings, outputfile='pecan.CONFIGS.xml')
#   PEcAn.utils::status.end()
# } else if (file.exists(file.path(settings$outdir, 'pecan.CONFIGS.xml'))) {
#   settings <- PEcAn.settings::read.settings(file.path(settings$outdir, 'pecan.CONFIGS.xml'))
# }
# print("---------- Wrtting Configs Completed ----------")



# Run state data assimilation
if ('state.data.assimilation' %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    settings <- PEcAn.assim.sequential::sda.enkf(settings,obs.list$obs.mean,obs.list$obs.cov)
    PEcAn.utils::status.end()
  }
}

