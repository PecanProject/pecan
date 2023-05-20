#!/usr/bin/Rscript --vanilla
# Modified from Alexis Helgeson's assim.sequential/inst/restart_SDAworkflow_scripts/SDA_Workflow_NA.R
#
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
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

# ----------------------------------------------------------------------------------------------
#------------------------------------------Prepared SDA Settings -----
# ----------------------------------------------------------------------------------------------
## parse start date
option_list = list(optparse::make_option("--start.date",
                                         default = Sys.Date(),
                                         type="character"),
                   optparse::make_option("--prev",
                                         default = paste0("/projectnb/dietzelab/dietze/hf_landscape_SDA/test02/FNA",Sys.Date()-lubridate::days(1)),
                                         type="character")
                   )
args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
#args$start.date = "2022-05-18 00:00:00"
#args$prev = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test02/FOF2022-05-17/"
start.date = lubridate::as_date(args$start.date)

#------------------------------------------------------------------------------------------------
#------------------------------------------ Preparing the pecan xml -----------------------------
#------------------------------------------------------------------------------------------------
restart <- list()
restart$filepath <- args$prev
set = readRDS("/projectnb/dietzelab/dietze/hf_landscape_SDA/test02/pecan.RDS")

#set met.start & met.end
end.date <- start.date + lubridate::days(35)
sda.start = start.date + lubridate::days(1)

# --------------------------------------------------------------------------------------------------
#---------------------------------------------- NA DATA -------------------------------------
# --------------------------------------------------------------------------------------------------

#initialize obs.mean/cov NAs
## TODO: Alexis's version had two dates, need to take a closer list at what dates these should be set to
site.ids <- papply(set,function(x)(x$run$site$id)) %>% unlist() %>% as.character()
nsite = length(site.ids)

NAdata = data.frame(date = c(rep(start.date,nsite),rep(sda.start,nsite)),
                    site_id = rep(site.ids,times=2),
                    data = rep(NA,nsite*2))
obs.mean <- obs.cov <- split(NAdata, NAdata$date)
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

obs.cov <- purrr::map(
  names(obs.cov),
  function(namesl){
    purrr::map(
      split(
        obs.cov[[namesl]],
        obs.cov[[namesl]]$site_id),
      ~.x[3]^2 %>%
        unlist %>%
        diag(nrow = 1, ncol = 1))
  }
) %>% stats::setNames(date.obs)

#add start.cut to restart list
restart$start.cut <- start.date
restart$start.cut <- format(restart$start.cut, "%Y-%m-%d %H:%M:%S", tz = "GMT")


#-----------------------------------------------------------------------------------------------
#------------------------------------------ Fixing the settings --------------------------------
#-----------------------------------------------------------------------------------------------
#Using the found dates to run - this will help to download mets
for(s in seq_along(set)){
  set[[s]]$run$start.date = start.date
  set[[s]]$run$end.date   = end.date
  set[[s]]$run$site$met.start = start.date
  set[[s]]$run$site$met.end   = end.date
}

# Setting dates in assimilation tags - This will help with preprocess split in SDA code
set$state.data.assimilation$start.date <-as.character(start.date)
set$state.data.assimilation$end.date <-as.character(max(names(obs.mean)))

# --------------------------------------------------------------------------------------------------
#---------------------------------------------- PEcAn Workflow -------------------------------------
# --------------------------------------------------------------------------------------------------
#info
set$info$date <- paste0(format(Sys.time(), "%Y/%m/%d %H:%M:%S"))
next.oldir <- paste0(format(Sys.time(), "%Y-%m-%d-%H-%M"))
#Update/fix/check settings. Will only run the first time it's called, unless force=TRUE
#set <- PEcAn.settings::prepare.settings(set, force = TRUE)
## TODO: make sure settings are prepared; for remote, make sure to set host directories

## outdirs
set$outdir = file.path(set$outdir,paste0("FNA",start.date,"/"))
set$rundir = file.path(set$outdir,"run")
set$modeloutdir = file.path(set$outdir,"out")
set$pfts$pft$outdir = file.path(set$outdir,"pft")
set$host$rundir <- set$rundir
set$host$outdir <- set$modeloutdir
set$host$folder <- set$modeloutdir
dir.create(set$outdir)
dir.create(set$rundir)
dir.create(set$modeloutdir)
dir.create(set$pfts$pft$outdir)

#manually add in clim files 
met_paths <- list.files(path = file.path("/projectnb/dietzelab/ahelgeso/NOAA_met_data_CH1/noaa_clim/HARV", start.date), full.names = TRUE, pattern = ".clim")
if(is_empty(met_paths)){
  print(paste("SKIPPING: NO MET FOR",start.date))
  cat(as.character(start.date),sep="\n",file=file.path(dirname(set$outdir),"NO_MET"),append=TRUE) ## add to list of dates missing met
  stop_quietly()
}
met_paths = as.list(met_paths)
names(met_paths) = rep("path",nsite)
for(s in seq_along(set)){
  set[[s]]$run$inputs$met$source = "GEFS" 
  set[[s]]$run$inputs$met$path = met_paths
}

#add run ids from previous sda to settings object to be passed to build X
prev_run_ids = list.files(file.path(restart$filepath, "out"))
run_id = as.data.frame(strsplit(prev_run_ids,"-")) %>% t()
colnames(run_id) <- c("pre","ens","site")
rownames(run_id) <- NULL
run_id = as.data.frame(run_id) %>% mutate(folder=prev_run_ids,id = paste0("id",.data$ens)) %>% group_by(site)
###settings$runs$id = run_id
for(s in seq_along(set)){
  site_run_id = run_id %>% filter(site == set[[s]]$run$site$id)
  set[[s]]$run$id =  as.list(site_run_id$folder)
  names(set[[s]]$run$id) = site_run_id$id
}

## job.sh
set$model$jobtemplate = "/projectnb/dietzelab/dietze/hf_landscape_SDA/test02/template.job"

#save restart object
save(restart, next.oldir, args, file = file.path(set$outdir, "restart.Rdata"))
#run sda function
sda.enkf.multisite(settings = set, 
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




