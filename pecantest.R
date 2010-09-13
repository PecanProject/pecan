setwd('/home/dlebauer/pecan/dev/R/')
library('RMySQL')

##the next 'source' lines will be replaced by library('PECAn')
source('query.bety.pft_species.R')
source('query.bety.priors.R') #given pft, query available priors
source('query.bety.traits.R') 
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

pft <- 'c4crop'

##edtraits is the comprehensive list of traits that could be in the database 
edtraits <- c('mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','leafN','water_conductance','Vm0','r_fract','storage_turnover_rate')

parms  <- query.bety.parameters(pft, edtraits)

prstr  <- vecpaste(parms)
priors <- query.bety.priors(pft, prstr)

trstr  <- vecpaste(gsub('Vm0', 'Vcmax', prstr))
data   <- query.bety.traits(pft, trstr)


##command to disconnect MySQL connections
lapply(dbListConnections(MySQL()), dbDisconnect)
>>>>>>> MERGE-SOURCE



##query.bety.traits()
query.bety.traits('PAVI2')
  # need to limit this to traits with priors

##query.bety.priors()
library('RMySQL')
source('query.bety.priors.R')
query.bety.traits('PAVI2')
  # need to limit this to traits with priors
