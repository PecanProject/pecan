### PROSPECT code and inversion, implemented in Nimble
library(data.table)
source("nimble/nimblefuncs/nim_setup.R")

### Load spectrum
if (!exists("TEST")){
        args <- commandArgs(trailingOnly=TRUE)
        specname <- args[1]
        ngibbs <- as.numeric(args[2])
        foldername <- args[3]
        runid <- args[4]
} else {
        specname <- "AK01_ACRU_M_LC_REFL"
        foldername <- "NIMBLE_TEST"
        runid <- "TEST"
        ngibbs <- 1000
}

speclist <- read.table("FFT_fullspecnames.txt", stringsAsFactors = FALSE)$V1
specindex <- grep(specname, speclist)
wd <- getwd()
print(sprintf("****** NAME: %s, WD: %s, INDEX: %d ********", specname, wd, specindex))
path.to.spec <- "../data/FFT_spectra/NASA_FFT_LC_Refl_Spectra_v4.csv"
obs.spec <- t(as.matrix(fread(path.to.spec,
                           nrows=1,
                           header=FALSE,
                           skip = specindex,
                           drop = 1:71)))
dir.create(sprintf("../run_results/%s", foldername), showWarnings = FALSE)
filename <- sprintf("../run_results/%s/%s_%s.dat", foldername, specname, runid)

AI <- 3

prospectConstants$observed <- obs.spec
prospectConstants$nspec <- ncol(obs.spec)
prospectConstants$wl <- nrow(obs.spec)

print("CREATING MODEL")
prospect <- nimbleModel(code = prospectCode,
                        name = "prospect",
                        constants = prospectConstants,
                        inits = prospectInits)

prospectNodes <- prospect$getNodeNames()

print("CONFIGURING MCMC")
prospectSpec <- configureMCMC(prospect, print=TRUE)
prospectSpec$removeSamplers(prospectNodes %in% prospectNodes)

### Load custom samplers
source("nimble/nimblefuncs/LL.R")
source("nimble/nimblefuncs/resp_long.R")

specialized_prospect_LL <- prospect_LL(prospect, prospectConstants)

sampler.pars <- c("Ni", "Cab", "Cw", "Cm")
for (i in sampler.pars){
        prospectSpec$addSampler(type = "RW_llFunction",
                                control = list(targetNode = i,
                                               llFunction = specialized_prospect_LL,
                                               includesTarget = FALSE,
                                               adaptInterval = AI))
}
prospectSpec$addSampler(type = "resp", control = list(targetNode = "resp"))

prospectSpec$addMonitors(c("N", "Cab", "Cw", "Cm", "resp"))
print("BUILDING MCMC")
prospectMCMC <- buildMCMC(prospectSpec, project = prospect)

prosProj <- compileNimble(prospect, prospectMCMC)

### Run MCMC
tt <- system.time(prosProj$prospectMCMC$run(ngibbs))
print(tt)
samples <- as.matrix(prosProj$prospectMCMC$mvSamples)[,c("N",
                                                         "Cab",
                                                         "Cw",
                                                         "Cm",
                                                         "resp")]

if(!exists("TEST")) {
        write.csv(samples, filename, row.names=FALSE)
} else {
        source("prospect.R")
        s1 <- data.table(samples)
        s1.mean <- s1[, sapply(.SD, mean)]
        s1.median <- s1[, sapply(.SD, median)]
        s1.mean.p <- do.call(prospect, as.list(s1.mean[-5]))
        s1.median.p <- do.call(prospect, as.list(s1.median[-5]))
        plot(obs.spec, type='l')
        lines(s1.mean.p, col=2)
        lines(s1.median.p, col=3)
        legend("topright", c("Obs", "Mean", "Med"), lty=1, col=1:3)
}

        

