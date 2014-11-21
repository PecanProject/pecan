## Run of Bayesian inversion of PROSPECT

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
ngibbs <- as.numeric(args[5])
runid <- args[6]
filename <- sprintf("run_results/%s_%g_%s_%s_%s_%s.dat", species, jrsd, precision, rearg, runid)

source("inv_bayes.R")
source("specdataproc.R")
specdat <- load.all.spec()

cond <- expression(Species == species & Spectra_Type == "Refl" & Wavelength >= 400)
smat <- specmatrix(specdat, cond)

pinvbayes(smat, ngibbs=ngibbs, JumpRSD=jrsd, fname=filename,
          local.store=FALSE, 
          single.precision=precision, 
          random.effects=rearg)
