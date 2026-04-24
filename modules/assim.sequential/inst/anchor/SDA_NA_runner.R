# loading libraries.
library(dplyr)
library(xts)
library(PEcAn.all)
library(purrr)
library(furrr)
library(lubridate)
library(nimble)
library(ncdf4)
library(PEcAnAssimSequential)
library(dplyr)
library(sp)
library(raster)
library(zoo)
library(ggplot2)
library(mnormt)
library(sjmisc)
library(stringr)
library(doParallel)
library(doSNOW)
library(Kendall)
library(lgarch)
library(parallel)
library(foreach)
library(terra)
setwd("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/")

# read settings xml file.
settings_dir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/pecan_new_obs_prep.xml"
settings <- PEcAn.settings::read.settings(settings_dir)

# update settings with the actual PFTs.
settings <- PEcAn.settings::prepare.settings(settings)

# setup the batch job settings.
general.job <- list(cores = 28, folder.num = 35)
batch.settings = structure(list(
  general.job = general.job,
  qsub.cmd = "qsub -l h_rt=24:00:00 -l mem_per_core=4G -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
))
settings$state.data.assimilation$batch.settings <- batch.settings

# alter the ensemble size.
settings$ensemble$size <- 100

# load observations.
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/observation/Rdata/obs.mean.Rdata")
load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/observation/Rdata/obs.cov.Rdata")

# replace zero observations and variances with small numbers.
for (i in 1:length(obs.mean)) {
  if(is.null(obs.mean[[i]][[1]])){
    next
  }
  for (j in 1:length(obs.mean[[i]])) {
    if (length(obs.mean[[i]][[j]])==0) {
      next
    }
    inds <- which(obs.mean[[i]][[j]]==0)
    for (ind in inds) {
      att <- attributes(obs.mean[[i]][[j]][[ind]])[[1]]
      obs.mean[[i]][[j]][[ind]] <- 0.01
      attr(obs.mean[[i]][[j]][[ind]], "source") <- att
    }
    if(length(obs.cov[[i]][[j]]) > 1){
      diag(obs.cov[[i]][[j]])[which(diag(obs.cov[[i]][[j]]<=0.1))] <- 0.1
    }else{
      if(obs.cov[[i]][[j]] <= 0.1){
        obs.cov[[i]][[j]] <- 0.1
      }
    }
  }
}

# load PFT parameter file.
load(file.path(settings$outdir, "samples.Rdata"))

# execute the SDA.
qsub_sda(settings = settings, 
         obs.mean = obs.mean, 
         obs.cov = obs.cov, 
         Q = NULL, 
         pre_enkf_params = NULL, 
         ensemble.samples = ensemble.samples, 
         outdir = NULL, 
         control = list(TimeseriesPlot = FALSE,
                        OutlierDetection=FALSE,
                        send_email = NULL,
                        keepNC = FALSE,
                        forceRun = TRUE,
                        MCMC.args = NULL,
                        merge_nc = TRUE),
         block.index = NULL,
         debias = list(cov.dir = "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/covariates_lc_ts/covariates_nolatlon/", 
                       t.start = 1, residual.lag = TRUE), prefix = "batch_Mar_10")

# export sda output.
PEcAnAssimSequential::sda_assemble("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/batch_Mar_10", 
                                   "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site")

# merge all netcdf files into single files across time steps.
batch.folder <- file.path(settings$outdir, "batch_Mar_10")
job.folders <- paste0("Job_", 1:as.numeric(settings$state.data.assimilation$batch.settings$general.job$folder.num))
time.points <- seq(as.Date(names(obs.mean)[1]), 
                   as.Date(names(obs.mean)[length(obs.mean)]),
                   paste(1, settings$state.data.assimilation$forecast.time.step)) %>% lubridate::year()
nc.outdir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/merged_nc"

# check which folder has not yet merged the nc files.
nc.files <- file.path(batch.folder, job.folders, "merged_nc", paste0(2024, ".nc"))
inds <- which(!file.exists(nc.files))
for (i in seq_along(inds)) {
  print(which(inds[i]==inds)/length(inds))
  configs <- readRDS(file.path(batch.folder, job.folders[inds[i]], "configs.rds"))
  nc.folder <- file.path(batch.folder, job.folders[inds[i]], "merged_nc")
  if (file.exists(nc.folder)) unlink(nc.folder, recursive = T)
  dir.create(nc.folder)
  temp <- PEcAn.utils::nc_merge_all_sites_by_year(model.outdir = file.path(batch.folder, job.folders[inds[i]], "out"), 
                                                  nc.outdir = nc.folder, 
                                                  ens.num = settings$ensemble$size, 
                                                  site.ids = as.numeric(configs$site.ids), 
                                                  start.date = names(obs.mean)[1], 
                                                  end.date = names(obs.mean)[length(obs.mean)], 
                                                  time.step = paste(1, settings$state.data.assimilation$forecast.time.step), 
                                                  cores = parallel::detectCores() - 1)
  
  # remove rundir and outdir.
  unlink(file.path(batch.folder, job.folders[inds[i]], "run"), recursive = T)
  unlink(file.path(batch.folder, job.folders[inds[i]], "out"), recursive = T)
}

# function for merging nc files.
merge_multi_nc_files <- function (nc.files, nc.outdir, cores) {
  # detect if we are merging the same file.
  fname <- unique(basename(nc.files))
  if (length(fname) != 1) {
    PEcAn.logger::logger.info("Files are not in the same name. Please check it!")
    return(0)
  }
  nc.out <- file.path(nc.outdir, fname)
  # merge across sites using CDO command.
  cmd <- "cdo -P @CORES@ collgrid @NC_FILES@ @NC.OUTDIR@"
  cmd <- gsub("@CORES@", cores, cmd)
  cmd <- gsub("@NC_FILES@", paste(nc.files, collapse = " "), cmd)
  cmd <- gsub("@NC.OUTDIR@", nc.out, cmd)
  out <- system(cmd, intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
}
# loop over time steps.
for (t in time.points) {
  nc.files <- file.path(batch.folder, job.folders, "merged_nc", paste0(t, ".nc"))
  # print(length(which(file.exists(nc.files))))
  merge_multi_nc_files(nc.files, nc.outdir, parallel::detectCores() - 1)
}

# debug mode.
configs <- readRDS("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/batch_Mar_10/Job_20/configs.rds")
settings <- PEcAn.settings::read.settings(configs$setting)
obs.mean <- configs$obs.mean
obs.cov <- configs$obs.cov
Q <- configs$Q 
pre_enkf_params <- configs$pre_enkf_params
ensemble.samples <- configs$ensemble.samples
outdir <- configs$outdir
control <- configs$control
debias <- configs$debias
sda_matchparam <- PEcAnAssimSequential:::sda_matchparam
build_X <- PEcAnAssimSequential:::build_X
analysis_sda_block <- PEcAnAssimSequential:::analysis_sda_block
py.init <- PEcAnAssimSequential:::.get_debias_mod
residual.lag <- TRUE
t.start <- 1
dates <- 2012:2024
cov.dir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/covariates_lc_ts/covariates_with_LAI"
