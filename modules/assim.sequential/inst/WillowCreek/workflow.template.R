# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------
library(PEcAn.all)
library(PEcAn.utils)
library(RCurl)
library(REddyProc)
library(purrr)
#------------------------------------------
setwd("/fs/data3/hamzed/pecan/modules/assim.sequential/inst/WillowCreek")
#setwd("/fs/data3/kzarada/pecan/modules/assim.sequential/inst/WillowCreek")
source('Utils.R')
source('download_WCr_flux.R')
source('download_WCr_met.R')
source("gapfill_WCr.R")
source('prep.data.assim.R')
outputPath <- "/fs/data3/kzarada/Projects/WillowCreek"
xmlPath <-"gefs.sipnet.template.xml"
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
sda.start <- as.Date("2018-05-15")
sda.end <- as.Date("2018-11-24")



prep.data <- prep.data.assim(sda.start, sda.end, numvals = 100, vars = c("NEE", "LE"), data.len = 72) 
#--------------------------- Preparing OBSdata
obs.raw <-prep.data$rawobs
prep.data<-prep.data$obs


met.start <- obs.raw$Date%>% head(1) %>% lubridate::floor_date(unit = "day")
met.end <- obs.raw$Date %>% tail(1) %>% lubridate::ceiling_date(unit = "day")

#--------- DOwnloading met
#met.raw <- download_US_WCr_met(met.start, met.end)
# Using the found dates to run - this will help to download mets
settings$run$start.date <- as.character(met.start)
settings$run$end.date <- as.character(met.end)
# Setting dates in assimilation tags - This will help with preprocess split in SDA code
settings$state.data.assimilation$start.date <-as.character(met.start)
settings$state.data.assimilation$end.date <-as.character(met.end)
#info
settings$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"), " +0000")
#-- Setting the out dir
settings$outdir <- file.path(outputPath, Sys.time() %>% as.numeric())
print(settings$outdir)
#--------- Making a plot
# obs.plot <- obs.raw %>%
#             tidyr::gather(Param, Value, -c(Date)) %>%
#             filter(!(Param %in% c("FjDay", "U","Day","DoY","FC","FjFay","Hour","Month",
#                                   "SC","Ustar","Year","H","Flag")),
#                    Value!=-999) %>%
#             #filter((Date %>% as.Date) %in% (names(prep.data) %>% as.Date())) %>%
#             ggplot(aes(Date, Value)) +
#             geom_line(aes(color = Param), lwd = 1) +
#             geom_point(aes(color = Param), size = 3) +
#             facet_wrap( ~ Param, scales = "free",ncol = 1) +
#             scale_x_datetime(breaks = scales::date_breaks("1 hour"),labels = scales::date_format("%j-%H"))+
#             scale_color_brewer(palette = "Set1") +
#             theme_minimal(base_size = 15) +
#             labs(y = "") +
#             theme(legend.position = "none",
#                   axis.text.x = element_text(angle = 90, hjust = 1))
# 
# # Make sure you have the premission - chmod is right
 if (!dir.exists(settings$outdir)) dir.create(settings$outdir)
# ggsave(file.path(settings$outdir,"Obs_plot.pdf"), obs.plot , width = 18, height = 10)


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
settings <- PEcAn.workflow::do_conversions(settings, T, T, T)

# # Query the trait database for data and priors
# if (PEcAn.utils::status.check("TRAIT") == 0) {
#   PEcAn.utils::status.start("TRAIT")
#   settings <- PEcAn.workflow::runModule.get.trait.data(settings)
#   PEcAn.settings::write.settings(settings, outputfile = 'pecan.TRAIT.xml')
#   PEcAn.utils::status.end()
# } else if (file.exists(file.path(settings$outdir, 'pecan.TRAIT.xml'))) {
#   settings <-
#     PEcAn.settings::read.settings(file.path(settings$outdir, 'pecan.TRAIT.xml'))
# }
# 
# 
# # Run the PEcAn meta.analysis
# if (!is.null(settings$meta.analysis)) {
#   if (PEcAn.utils::status.check("META") == 0) {
#     PEcAn.utils::status.start("META")
#     PEcAn.MA::runModule.run.meta.analysis(settings)
#     PEcAn.utils::status.end()
#   }
# }
# 
# 
# # Write model specific configs
# # if (PEcAn.utils::status.check("CONFIG") == 0){
# #   PEcAn.utils::status.start("CONFIG")
# #   settings <- runModule.run.write.configs(settings)
# #   PEcAn.settings::write.settings(settings, outputfile='pecan.CONFIGS.xml')
# #   PEcAn.utils::status.end()
# # } else if (file.exists(file.path(settings$outdir, 'pecan.CONFIGS.xml'))) {
# #   settings <- PEcAn.settings::read.settings(file.path(settings$outdir, 'pecan.CONFIGS.xml'))
# # }
# # print("---------- Wrtting Configs Completed ----------")
# 
obs.mean <-prep.data %>% map('means') %>% setNames(names(prep.data))
obs.cov <- prep.data %>% map('covs') %>% setNames(names(prep.data))
# 
# 
# 
# # Run state data assimilation
if ('state.data.assimilation' %in% names(settings)) {
  if (PEcAn.utils::status.check("SDA") == 0) {
    PEcAn.utils::status.start("SDA")
    settings <- PEcAn.assim.sequential::sda.enkf(
      settings,
      obs.mean = obs.mean,
      obs.cov = obs.cov,
      control = list(
        trace = T,
        interactivePlot =F,
        TimeseriesPlot =T,
        BiasPlot =F,
        plot.title =paste("WCr SDA for between ", start_date, "to", end_date),
        debug = T
      )
    )
    PEcAn.utils::status.end()
  }
}
