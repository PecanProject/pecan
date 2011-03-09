require(XML)
setwd('~/pecan')  
settings.file <- "~/pecan/settings.xml"
#settings.file <- system("echo $PECANSETTINGS", intern = TRUE)

settings.xml <- xmlTreeParse(settings.file)
settings <- xmlToList(settings.xml)
if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 

pft                  <- settings$pft
ma_iter              <- as.numeric(settings$ma_iter)
ensemble_size        <- as.numeric(settings$ensemble_size)
sensitivity_analysis <- as.logical(settings$sensitivity_analysis)
outdir               <- settings$outdir
require(PECAn)

## connect to database
con <- settings$database
if(settings$database$location == 'localhost'){
  con <- query.bety.con(dbname=con$name,password=con$passwd,username=con$userid)
}

## 1. get species list based on pft
spstr <- query.bety.pft_species(pft, con=con)


## 2. get priors available for pft
## trstr is a list of the traits that ED can use
  trstr <- "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA_m2_per_gC','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','c2n_leaf','water_conductance','Vm0','r_fract','storage_turnover_rate'" #SLA_gC_per_m2 is converted to SLA in query.bety.priors

priors <- query.bety.priors(pft, trstr,out=outdir,con=con)

traits <- rownames(priors) # vector of traits with prior distributions for pft 

trait.defs <- trait.dictionary(traits)

## now it is time to query the data
trait.data <- query.bety.traits(spstr, vecpaste(traits), con=con) 
## returns list 'trait.data' with one dataframe per variable 

## run the meta-analysis
trait.mcmc <- pecan.ma(trait.data, priors, taupriors, j.iter = ITER, settings)
pecan.ma.summary(trait.mcmc, pft,outdir)
outfile <- paste(outdir, '/pecan.MA.Rdata', sep = '')
save.image(outfile)

