library(PECAn, lib.loc = '~/lib/R')
load('pecan.start.Rdata')
## 1. get species list based on pft
spp <- query.bety.pft_species(pft)
spstr <- spp$spstr 

## 2. get priors available for pft
priors <- query.bety.priors(pft)
print(priors)

prvec <- rownames(priors) # vector of traits with prior distributions for pft 
prstr <- vecpaste(prvec)  # string of " " " " used to query priors

trvec <- gsub('Vm0', 'Vcmax', prvec)  


## now it is time to query the data
trait.data <- query.bety.traits(spstr,trvec) 
## returns list 'trait.data' with one dataframe per variable 

## run the meta-analysis
trait.mcmc <- pecan.ma(trait.data, priors, j.iter = ITER)

pecan.ma.summary(trait.mcmc, pft)
save.image('pecan.MA.Rdata')
