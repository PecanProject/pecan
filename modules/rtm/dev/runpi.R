##' Run of Bayesian inversion of PROSPECT
##'
##' For initializing inversions from the command line, e.g. for submission to cluster.
##' This script takes 8 command line arguments, which are as follows:
##'   [1] Spectype.Species (e.g. SE.QUCH)
##'   [2] Random effects; current options are 'none' and 'leaf'
##'   [3] How to generate initial conditions; OPTIONS: 'random', 'mle' (R optim function for mean observed spectrum), 'guess' (preset values)
##'   [4] Number of MCMC steps.
##'   [5] Filename tag to identify run results
##'   [6] Sub-folder in "run_results" for storing output.

#setwd("~/Documents/Unsynced/pecan/modules/rtm/R")
options(warn=2)
source("inversion_bayes.R")
source("input_matrix.R")

args <- commandArgs(trailingOnly=TRUE)
specarg <- strsplit(args[1], "\\.")[[1]]
spectra <- specarg[1]
species <- specarg[2]
rearg <- args[2]
initarg <- args[3]
ngibbs <- as.numeric(args[4])
runid <- args[5]
folder <- args[6]
filename <- sprintf("../run_results/%s/%s_%s_%s_%s.dat", folder, species, rearg, initarg, runid)
dir.create(sprintf("../run_results/%s", folder), showWarnings = FALSE)

smat <- specmatrix(species, spectype=spectra)

pinvbayes(smat,
          ngibbs=ngibbs,
          fname=filename,
          random.effects=rearg,
          inits=initarg,
          ar.step=100)

