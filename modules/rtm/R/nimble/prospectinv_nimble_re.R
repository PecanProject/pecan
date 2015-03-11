### PROSPECT code and inversion, implemented in Nimble
library(data.table)
source("nimble/nimblefuncs/nim_setup_re.R")

### Load spectrum
if (!exists("TEST")){
        args <- commandArgs(trailingOnly=TRUE)
        specname <- args[1]
        ngibbs <- as.numeric(args[2])
        foldername <- args[3]
        runid <- args[4]
} else {
        specname <- "JUNI"
        foldername <- "NIMBLE_TEST_RE"
        runid <- "TEST_RE"
        ngibbs <- 100
}

path.to.spec <- "../data/FFT_spectra/NASA_FFT_LC_Refl_Spectra_v4.csv"
fullspec <- fread(path.to.spec, header=TRUE, drop=22:71)
obs.spec <- t(as.matrix(fullspec[Species == specname, 22:ncol(fullspec),
                                 with=FALSE]))
leaf.names <- fullspec[Species == specname,
                     sprintf("%s_%s", Spectra, Sample_Year)]
dir.create(sprintf("../run_results/%s", foldername), showWarnings = FALSE)
filename <- sprintf("../run_results/%s/%s_%s.dat", foldername, specname, runid)

AI <- 3

prospectConstants$observed <- obs.spec
prospectConstants$nspec <- ncol(obs.spec)
prospectConstants$wl <- nrow(obs.spec)

prospect <- nimbleModel(code = prospectCode,
                        name = "prospect",
                        constants = prospectConstants,
                        inits = prospectInits)

prospectNodes <- prospect$getNodeNames()

prospectSpec <- configureMCMC(prospect, print=TRUE)
prospectSpec$removeSamplers(prospectNodes %in% prospectNodes)

### Load custom samplers
source("nimble/nimblefuncs/LL_re.R")
source("nimble/nimblefuncs/resp_long_re.R")

specialized_prospect_LL <- prospect_LL(prospect, prospectConstants)

sampler.pars <- c("mu.N", "mu.Cab", "mu.Cw", "mu.Cm")
random.effects <- sprintf("alpha.%s[%d]",
                          rep(c("N", "Cab", "Cw", "Cm"), each=ncol(obs.spec)),
                          1:ncol(obs.spec))
                          
for (i in c(sampler.pars, random.effects)){
        prospectSpec$addSampler(type = "RW_llFunction",
                                control = list(targetNode = i,
                                               llFunction = specialized_prospect_LL,
                                               includesTarget = FALSE,
                                               adaptInterval = AI))
}
prospectSpec$addSampler(type = "resp", control = list(targetNode = "resp"))

prospectSpec$addMonitors(c("alpha.N", "alpha.Cab", "alpha.Cw", "alpha.Cm"))
prospectMCMC <- buildMCMC(prospectSpec, project = prospect)

prosProj <- compileNimble(prospect, prospectMCMC)

### Run MCMC
timer <- system.time(prosProj$prospectMCMC$run(ngibbs))
samples <- as.matrix(prosProj$prospectMCMC$mvSamples)

write.csv(samples, filename, row.names=FALSE)

