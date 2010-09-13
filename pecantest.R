setwd('/home/dlebauer/pecan/dev/R/')
library('RMySQL')

##the next 'source' lines will be replaced by library('PECAn')
source('query.bety.pft_species.R')
source('query.bety.priors.R') #given pft, query available priors
source('query.bety.traits.R') 
source('vecpaste.R')
source('query.bety.con.R')

##input variables
pft <- 'ebifarm.nfixer'


## 1. get species list based on pft
spp <- query.bety.pft_species(pft)
spstr <- vecpaste(spp$plant_id)

## 2. get priors available for pft
priors <- query.bety.priors(pft)

prvec <- priors$VarID     # vector of traits with prior distributions for pft 
prstr <- vecpaste(prvec)  # string of " " " " used to query priors

trvec <- gsub('Vm0', 'Vcmax', prvec)  
trstr <- gsub('Vm0', 'Vcmax', prstr) #used to query trait data



##query.bety.traits()
query.bety.traits('PAVI2')
  # need to limit this to traits with priors

##query.bety.priors()
library('RMySQL')
source('query.bety.priors.R')
query.bety.traits('PAVI2')
  # need to limit this to traits with priors
