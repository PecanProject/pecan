##' Run of Bayesian inversion of PROSPECT
##'
##' For initializing inversions from the command line, e.g. for submission to cluster.
##' This script takes 8 command line arguments, which are as follows:
##'   [1] Starting SD of Jump distribution; larger values indicate larger jumps
##'   [2] Species; must EXACTLY match species tag in spectra filenames
##'   [3] Whether to use a single precison value for all wavelengths (1) or an individual precision at each wavelength (0)
##'   [4] Random effects; current options are 'none' and 'leaf'
##'   [5] How to generate initial conditions; OPTIONS: 'random', 'mle' (R optim function for mean observed spectrum), 'guess' (preset values)
##'   [6] Number of MCMC steps.
##'   [7] Filename tag to identify run results
##'   [8] Sub-folder in "run_results" for storing output.

#setwd("~/Documents/Unsynced/pecan/modules/rtm/R")
source("inversion_bayes.R")
source("input_matrix.R")

args <- commandArgs(trailingOnly=TRUE)
jrsd <- as.numeric(args[1])
species <- args[2]
precarg <- as.numeric(args[3])
if(precarg){
        precision <- "sp"
} else {
        precision <- "pwl"
}
rearg <- args[4]
initarg <- args[5]
ngibbs <- as.numeric(args[6])
runid <- args[7]
folder <- args[8]
filename <- sprintf("../run_results/%s/%s_%g_%s_%s_%s_%s.dat", folder, species, jrsd, precision, rearg, initarg, runid)
dir.create(sprintf("../run_results/%s", folder), showWarnings = FALSE)

smat <- specmatrix(species)

pinvbayes(smat, ngibbs=ngibbs, JumpRSD=jrsd, fname=filename,
          local.store=FALSE, 
          single.precision=precarg, 
          random.effects=rearg,
          inits=initarg,
          ar.step=100)

