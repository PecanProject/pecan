setwd('/home/dlebauer/pecan/dev/R/')
library('RMySQL')

##the next 'source' lines will be replaced by library('PECAn')
source('query.bety.pft_species.R')
source('query.bety.priors.R') #given pft, query available priors
source('vecpaste.R')
source('query.bety.con.R')

##input variables
pft <- 'ebifarm.c4crop'


## 1. get species list based on pft
spp <- query.bety.pft_species(pft)
spstr <- spp$spstr

## 2. get priors available for pft
priors <- query.bety.priors(pft)

prvec <- rownames(priors) # vector of traits with prior distributions for pft 
prstr <- vecpaste(prvec)  # string of " " " " used to query priors

trvec <- gsub('Vm0', 'Vcmax', prvec)  
trstr <- gsub('Vm0', 'Vcmax', prstr) #used to query trait data

#objects:
# "pft" "priors" "prstr" "prvec" "query.bety.con" "query.bety.pft_species"
# "query.bety.priors" "spp" "spstr" "trstr" "trvec" "vecpaste"              

## now it is time to query the data 
