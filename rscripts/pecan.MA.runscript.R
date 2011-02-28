require(XML)
#settings.file = "~/pecan/settings.xml"
settings.file <- system("echo $PECANSETTINGS", intern = TRUE)
settings.xml = xmlTreeParse(settings.file)
settings = xmlToList(settings.xml)
if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
#pft   <- system("echo $PFT", intern = TRUE)
#ITER  <- as.numeric(system("echo $ITER", intern = TRUE)) 
#M     <- as.numeric(system("echo $ENSN", intern = TRUE))
#outdir   <- system("echo $PECANOUT", intern = TRUE)
pft    = settings$pft
ITER   = as.numeric(settings$ma_iter)
M      = as.numeric(settings$ensemble_size)
outdir = settings$outdir
outfile1 <- paste(outdir, '/pecan.parms.Rdata', sep = '')
save.image(outfile1)

require(PECAn)
#load('out/pecan.parms.Rdata') # for use from inside R 

## connect to database
con = settings$database
if(settings$database$location == 'localhost'){
  con <- query.bety.con(dbname=con$name,password=con$passwd,username=con$userid)
}

## 1. get species list based on pft
spstr <- query.bety.pft_species(pft,con=con)


## 2. get priors available for pft
## trstr is a list of the traits that ED can use
  trstr <- "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA_gC_per_m2','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','c2n_leaf','water_conductance','Vm0','r_fract','storage_turnover_rate'" #SLA_gC_per_m2 is converted to SLA in query.bety.priors

priors <- query.bety.priors(pft, trstr,out=outdir,con=con)
print(priors)

#prior.variances <- data.frame(var = unlist(t(sapply(1:nrow(priors), function(i) with(priors[i,], pdf.stats(distn, parama, paramb)))['var',])), row.names = rownames(priors))
prior.variances = as.data.frame(rep(1,nrow(priors)))  ######## HACK ###########
row.names(prior.variances) = row.names(priors)

taupriors <- list(tauA = 0.01,
                  tauB = apply(prior.variances, 1, function(x) min(0.01, x)))


trvec <- rownames(priors) # vector of traits with prior distributions for pft 

traits <- trvec
trait.defs <- trait.dictionary(trvec)

save(trait.defs, file = paste(outdir, '/trait.defs.Rdata', sep=''))
## now it is time to query the data
trait.data <- query.bety.traits(spstr,trvec,con=con) 
## returns list 'trait.data' with one dataframe per variable 

## run the meta-analysis
trait.mcmc <- pecan.ma(trait.data, priors, taupriors,j.iter = ITER,settings)

pecan.ma.summary(trait.mcmc, pft,outdir)
outfile2 <- paste(outdir, '/pecan.MA.Rdata', sep = '')
save.image(outfile2)
#save(outdir, file='outdir.Rdata')
