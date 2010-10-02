setwd('/home/dlebauer/pecan/dev/R/')
library(PECAn, lib.loc = '~/lib/R')
##input variables
pft <- 'ebifarm.c4crop'

## 1. get species list based on pft
spp <- query.bety.pft_species(pft)
spstr <- spp$spstr

## 2. get priors available for pft
priors <- query.bety.priors(pft)
print(priors)

prvec <- rownames(priors) # vector of traits with prior distributions for pft 
prstr <- vecpaste(prvec)  # string of " " " " used to query priors

trvec <- gsub('Vm0', 'Vcmax', prvec)  

#objects:
# "pft" "priors" "prstr" "prvec" "query.bety.con" "query.bety.pft_species"
# "query.bety.priors" "spp" "spstr" "trstr" "trvec" "vecpaste"              

## now it is time to query the data
trait.data <- query.bety.traits(spstr,trvec) 
## returns list 'trait.data' with one dataframe per variable 

## run the meta-analysis
trait.mcmc <- pecan.ma(trait.data, n.iter = 1000)

## print out some statistical summaries and figures from meta-analysis
lapply(ma.summary, to.be.named.output)

## sample values for ensemble
samps <- pecan.samps(trait.mcmc, priors)
priors <- samps[['priors']]
trait.mat <- samps[['trait.mat']]

## generate config files
lapply(to.be.named.output, write.config.files)









################################
## This is where I am testing the validity of the stat. conversions in query.bety.traits() 
###############################
## testing SE = f(P)

#write.csv(data,'query.bety.traits.test.csv') 
data <- read.csv('query.bety.traits.test.csv')

#data <-  data.frame(site = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,5), mean = c(1,1,1,1.1,1,2,3,4,1,1,1,10,10000,0,0,0,5,5,5,5,5), statname = c(rep('SD',8),rep('none',4), rep('SD',4),'MSE','95%CI','LSD','HSD','MSD'), stat = c(1:16,rep(1,5)), n = rep(4,21))

data <- transform(data,
                  stat = as.numeric(stat),
                  n    = as.numeric(n))
          
