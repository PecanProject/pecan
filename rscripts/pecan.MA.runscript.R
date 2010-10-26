library(PECAn, lib.loc = '~/lib/R')
pft   <- system("echo $PFT", intern = TRUE)
ITER  <- as.numeric(system("echo $ITER", intern = TRUE)) 
M     <- as.numeric(system("echo $ENSN", intern = TRUE))
outdir   <- system("echo $PECANOUT", intern = TRUE)
outfile1 <- paste(outdir, '/pecan.parms.Rdata', sep = '')
save.image(outfile1)
 
print(cat("PECAn run with ",pft,
          "\nmeta-analysis has",ITER,"iterations",
          "\nensemble has",M,"config files"))


## 1. get species list based on pft
spp <- query.bety.pft_species(pft)
spstr <- spp$spstr 

## 2. get priors available for pft
priors <- query.bety.priors(pft)
print(priors)

prvec <- rownames(priors) # vector of traits with prior distributions for pft 
prstr <- vecpaste(prvec)  # string of " " " " used to query priors

trvec <- gsub('Vm0', 'Vcmax', prvec)  
traits <- trvec
trait.defs <- trait.dictionary(traits)
save(trait.defs, file = paste(outdir, '/trait.defs.Rdata', sep=''))
## now it is time to query the data
trait.data <- query.bety.traits(spstr,trvec) 
## returns list 'trait.data' with one dataframe per variable 

## run the meta-analysis
trait.mcmc <- pecan.ma(trait.data, priors, j.iter = ITER)

pecan.ma.summary(trait.mcmc, pft)
outfile2 <- paste(outdir, 'pecan.MA.Rdata', sep = '')
save.image(outfile2)
