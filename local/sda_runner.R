# loading libraries
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
library(data.table)
library(Kendall)
library(lgarch)
library(parallel)
library(foreach)
library(terra)
setwd("/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/wishart_sda/")
## read settings xml file.
load("/projectnb/dietzelab/guYANG/pecan/runners/test10/pecan_flux.RData")
# Change dir name and settings
settings$outdir      <- "/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/output_inter_q_2/"
settings$rundir      <- file.path(settings$outdir, "run")
settings$modeloutdir <- file.path(settings$outdir, "out")
settings$host$rundir <- file.path(settings$outdir, "run")
settings$host$outdir <- file.path(settings$outdir, "out")
settings$host$folder <- file.path(settings$outdir, "out")
settings$ensemble$size <- 5
settings$state.data.assimilation$adjustment <- "TRUE"
settings$host$prerun <- "module load R/4.4.0"
###### Change Q type
settings$state.data.assimilation$q.type <- "wishart"
## Fix the multi output in one timestep bug
settings$model$jobtemplate <- "/projectnb/dietzelab/guYANG/pecan/runners/test7/sipnet_template.job"

# Load the selected sites
load("/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/sda_idx.Rdata")
all_ids  <- vapply(settings, \(s) as.character(s$run$site$id), "")
# Sub settings for this run
settings <- settings[all_ids %in% keep_ids]
settings <- PEcAn.settings::as.MultiSettings(settings)

# setup the batch job settings.
general.job <- list(cores = 28, folder.num = 80)
batch.settings = structure(list(
  general.job = general.job,
  qsub.cmd = "qsub -l h_rt=24:00:00 -l mem_per_core=4G -l buyin -pe omp @CORES@ -V -N @NAME@ -o @STDOUT@ -e @STDERR@ -S /bin/bash"
))
settings$state.data.assimilation$batch.settings <- batch.settings

# update settings with the actual PFTs.
settings <- PEcAn.settings::prepare.settings(settings)

# load 6 obs
load("/projectnb/dietzelab/guYANG/pecan/runners/test10/obs.mean.RData")
load("/projectnb/dietzelab/guYANG/pecan/runners/test10/obs.cov.RData")

# load 4 obs
# load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/observation/Rdata_with_attributes/obs.mean.LandTrendr_GEDI.Rdata")
# load("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/observation/Rdata_with_attributes/obs.cov.LandTrendr_GEDI.Rdata")

sub_obs <- function(L, keep) setNames(lapply(L, \(l) l[names(l) %in% keep]), names(L))
obs.mean <- sub_obs(obs.mean, keep_ids)
obs.cov  <- sub_obs(obs.cov,  keep_ids)

# replace zero observations and variances with small numbers.
for (i in 1:length(obs.mean)) {
  if(is.null(obs.mean[[i]][[1]])){
    next
  }
  for (j in 1:length(obs.mean[[i]])) {
    if (length(obs.mean[[i]][[j]])==0) {
      next
    }
    obs.mean[[i]][[j]][which(obs.mean[[i]][[j]]==0)] <- 0.01
    if(length(obs.cov[[i]][[j]]) > 1){
      diag(obs.cov[[i]][[j]])[which(diag(obs.cov[[i]][[j]]<=0.1))] <- 0.1
    }else{
      if(obs.cov[[i]][[j]] <= 0.1){
        obs.cov[[i]][[j]] <- 0.1
      }
    }
  }
}

if (length(obs.cov[[i]][[j]]) > 1) {
  d <- diag(obs.cov[[i]][[j]])
  d[d <= 0.1] <- 0.1
  diag(obs.cov[[i]][[j]]) <- d
}

# load PFT parameter file.
samples_src <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/samples.Rdata"
samples_dst <- file.path(settings$outdir, "samples.Rdata")
dir.create(settings$outdir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(samples_dst)) file.copy(samples_src, samples_dst, overwrite = TRUE)

control <- list(
  TimeseriesPlot = FALSE,
  OutlierDetection = FALSE,
  send_email = NULL,
  keepNC = TRUE,
  forceRun = TRUE,
  run_parallel = FALSE,
  MCMC.args = NULL,
  merge_nc = FALSE,
  execution = "qsub_parallel"   # or "qsub" / "local"
)

source("/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/wishart_sda/sda.enkf_local.R")
source("/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/wishart_sda/read.restart.SIPNET.R")
source("/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/wishart_sda/analysis_sda_block.R")
source("/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/wishart_sda/MCMC_block_function.R")
# source("/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/wishart_sda/sda.enkf_param.R")

###### Be careful: param
res <- PEcAnAssimSequential:::sda.enkf_local(
  settings = settings,
  obs.mean = obs.mean,
  obs.cov = obs.cov,
  Q = NULL,
  pre_enkf_params = NULL,
  ensemble.samples = NULL,
  control = control
)

# job_lines <- c(
#   "#!/bin/bash",
#   "module load R/4.4.0",
#   "Rscript /projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/wishart_sda/pecan_sda_runner.R"
# )
# writeLines(job_lines, "/projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/logs/pecan_sda_runner.sh")

# qsub -l h_rt=2:00:00 \
# -l buyin \
# -l mem_per_core=8G \
# -pe omp 28 \
# -V \
# -N pecan_sda_runner \
# -o /projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/logs/pecan_sda_runner.out \
# -e /projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/logs/pecan_sda_runner.err \
# -M yanggu@bu.edu \
# -m abe \
# -S /bin/bash \
# /projectnb/dietzelab/guYANG/pecan/runners/wishart_sda/logs/pecan_sda_runner.sh
