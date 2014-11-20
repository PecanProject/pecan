## Run of Bayesian inversion of PROSPECT

args <- commandArgs(trailingOnly=TRUE)
jrsd <- as.numeric(args[1])
species <- args[2]
precision <- as.numeric(args[3])
ngibbs <- as.numeric(args[4])
filename <- sprintf("run_results/%s_%g_%s_PlotRE.dat", species, jrsd, as.character(precision>0))

source("inv_bayes.R")
source("specdataproc.R")
specdat <- load.all.spec()

cond <- expression(Species == species & Spectra_Type == "Refl" & Wavelength >= 400)
smat <- specmatrix(specdat, cond)

pinvbayes(smat, ngibbs=ngibbs, JumpRSD=jrsd, fname=filename,
          local.store=FALSE, 
          single.precision=precision)
