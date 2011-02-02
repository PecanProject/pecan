require(PECAn)
pft   <- system("echo $PFT", intern = TRUE)
ITER  <- as.numeric(system("echo $ITER", intern = TRUE)) 
M     <- as.numeric(system("echo $ENSN", intern = TRUE))
outdir   <- system("echo $PECANOUT", intern = TRUE)
outfile1 <- paste(outdir, '/pecan.parms.Rdata', sep = '')
save.image(outfile1)
#load('out/pecan.parms.Rdata') # for use from inside R 

## 1. get species list based on pft
spp <- query.bety.pft_species(pft)
spstr <- spp$spstr 



## 2. get priors available for pft
## trstr is a list of the traits that ED can use
  trstr <- "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA_gC_per_m2','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','c2n_leaf','water_conductance','Vm0','r_fract','storage_turnover_rate'" #SLA_gC_per_m2 is converted to SLA in query.bety.priors

priors <- query.bety.priors(pft, trstr,out=outdir)
print(priors)

trvec <- rownames(priors) # vector of traits with prior distributions for pft 

traits <- trvec
trait.defs <- trait.dictionary(trvec)

save(trait.defs, file = paste(outdir, '/trait.defs.Rdata', sep=''))
## now it is time to query the data
trait.data <- query.bety.traits(spstr,trvec) 
## returns list 'trait.data' with one dataframe per variable 

## run the meta-analysis
trait.mcmc <- pecan.ma(trait.data, priors, j.iter = ITER)

pecan.ma.summary(trait.mcmc, pft)
outfile2 <- paste(outdir, '/pecan.MA.Rdata', sep = '')
save.image(outfile2)
save(outdir, file='outdir.Rdata')
