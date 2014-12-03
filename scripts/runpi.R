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
initarg <- args[5]
ngibbs <- as.numeric(args[6])
runid <- args[7]
filename <- sprintf("run_results/%s_%g_%s_%s_%s_%s.dat", species, jrsd, precision, rearg, initarg, runid)

source("inv_bayes.R")
source("specdataproc.R")
specdat <- load.all.spec()

cond <- expression(Species == species & Spectra_Type == "Refl" & Wavelength >= 400)
smat <- specmatrix(specdat, cond)

pinvbayes(smat, ngibbs=ngibbs, JumpRSD=jrsd, fname=filename,
          local.store=FALSE, 
          single.precision=precarg, 
          random.effects=rearg,
          inits=initarg)
