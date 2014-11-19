## Run of Bayesian inversion of PROSPECT

args <- commandArgs(trailingOnly=TRUE)
jrsd <- as.numeric(args[1])
species <- args[2]
together <- as.numeric(args[3])
filename <- sprintf("run_results/%s_%g_%s.dat", species, jrsd, as.character(together>0))

source("inv_bayes.R")
source("specdataproc.R")
specdat <- load.all.spec()

cond <- expression(Species == species & Spectra_Type == "Refl" & Wavelength >= 400)
smat <- specmatrix(specdat, cond)

pinvbayes(smat, ngibbs=1e5, JumpRSD=jrsd, fname=filename,
          local.store=FALSE, sample.together=together)
