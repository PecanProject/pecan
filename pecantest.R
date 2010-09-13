setwd('/home/dlebauer/pecan/dev/R/')
library('RMySQL')

##the next 'source' lines will be replaced by library('PECAn')
source('query.bety.traits.R')
source('query.bety.priors.R')


##input variables
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



##query.bety.traits()
query.bety.traits('PAVI2')
  # need to limit this to traits with priors

##query.bety.priors()
library('RMySQL')
source('query.bety.priors.R')
query.bety.traits('PAVI2')
  # need to limit this to traits with priors
