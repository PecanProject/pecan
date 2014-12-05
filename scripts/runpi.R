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
folder <- args[8]
filename <- sprintf("run_results/%s/%s_%g_%s_%s_%s_%s.dat", folder, species, jrsd, precision, rearg, initarg, runid)
dir.create(sprintf("run_results/%s", folder), showWarnings = FALSE)

source("inv_bayes.R")
source("specdataproc.R")

smat <- specmatrix(species)

pinvbayes(smat, ngibbs=ngibbs, JumpRSD=jrsd, fname=filename,
          local.store=FALSE, 
          single.precision=precarg, 
          random.effects=rearg,
          inits=initarg,
          ar.step=100)
