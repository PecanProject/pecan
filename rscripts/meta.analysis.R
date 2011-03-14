
library(XML)
#settings.file = '~/pecan/tundra.grass.xml'
settings.file <- system("echo $PECANSETTINGS", intern = TRUE)

settings.xml <- xmlTreeParse(settings.file)
settings <- xmlToList(settings.xml)

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
#pft   <- system("echo $PFT", intern = TRUE)
#ITER  <- as.numeric(system("echo $ITER", intern = TRUE)) 
#M     <- as.numeric(system("echo $ENSN", intern = TRUE))
#outdir   <- system("echo $PECANOUT", intern = TRUE)
ITER   = as.numeric(settings$ma_iter)
M      = as.numeric(settings$ensemble_size)

## trstr is a list of the traits that ED can use

trstr <- "'mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','c2n_leaf','water_conductance','Vm0','r_fract','storage_turnover_rate','agf_bs'" #SLA_gC_per_m2 is converted to SLA in query.bety.priors

trait.name = strsplit(trstr,",")
trait.name = sub("'","",trait.name[[1]])
trait.name = sub("'","",trait.name)
trait.name2 = sub("Vm0","Vcmax",trait.name)
n.trait = length(trait.name)

ma_iter   = as.numeric(settings$ma_iter)
ensemble_size = as.numeric(settings$ensemble_size)
sensitivity_analysis = as.logical(settings$sensitivity_analysis)


require(PECAn)

## connect to database
con <- settings$database
if(settings$database$location == 'localhost'){
  con <- query.bety.con(dbname=con$name,password=con$passwd,username=con$userid)
}


## identify pfts
pfts   = which(names(settings) == 'pft'); pft.name = sapply(settings[pfts],function(x){x$name})
npft   = length(pfts);
if(npft < 1 | is.null(npft)) stop('no PFT specified')
mtemp = matrix(NA,n.trait,npft);row.names(mtemp) = trait.name; colnames(mtemp)=pft.name
pft.summary <- list(mean = mtemp,sd=mtemp,n=mtemp)


### loop over pfts
for( i in 1:length(pfts)){

  pft    = settings[[pfts[i]]]$name
  outdir = settings[[pfts[i]]]$outdir
  outfile1 <- paste(outdir, '/pecan.parms.Rdata', sep = '')
  save.image(outfile1)
  
  
  ## 1. get species list based on pft
  spstr <- query.bety.pft_species(pft,con=con)
  
  
  ## 2. get priors available for pft  
  priors <- query.bety.priors(pft, trstr,out=outdir,con=con)
  print(priors)
  

  traits <-trvec <- rownames(priors) # vector of traits with prior distributions for pft 
  trait.defs <- trait.dictionary(trvec)
  save(trait.defs, file = paste(outdir, '/trait.defs.Rdata', sep=''))
  
  ## now it is time to query the data
  ## returns list 'trait.data' with one dataframe per variable
  trait.data <- query.bety.traits(spstr,trvec,con=con) 

  ## DATA HACKS **** THESE SHOULD BE FIXED IN THE DATABASE*******
  if("root_respiration_factor" %in% names(trait.data)){
    sel = which(trait.data[["root_respiration_factor"]]$Y < 0.05)
    trait.data[["root_respiration_factor"]]$Y[sel] = trait.data[["root_respiration_factor"]]$Y[sel]*1000
  }
  if("SLA" %in% names(trait.data)){
    sel = which(trait.data[["SLA"]]$citation_id %in% c(311))
    if(length(sel) > 0){
      trait.data[["SLA"]] = trait.data[["SLA"]][-sel,]            
    }
  }
  save(trait.data, file = paste(outdir, '/trait.data.Rdata', sep=''))

  trait.count <- sapply(trait.data,nrow)
  trait.average <- sapply(trait.data,function(x){mean(x$Y,na.rm=TRUE)}); names(trait.average)[names(trait.average)=="Vcmax"] = "Vm0"
  pft.summary$n[match(names(trait.count),trait.name2),i] = trait.count
  

  ##prior.variances <- data.frame(var = unlist(t(sapply(1:nrow(priors), function(i) with(priors[i,], pdf.stats(distn, parama, paramb)))['var',])), row.names = rownames(priors))
  prior.variances = as.data.frame(rep(1,nrow(priors)))
  row.names(prior.variances) = row.names(priors)
  prior.variances[names(trait.average),] = 0.001*trait.average^2 

  ## Set gamma distribution prior on
#  prior.var <- function(x) do.call(pdf.stats, list(x$distn, x$parama, x$paramb))['var']
#  prior.variances <- data.frame(var = sapply(1:nrow(priors), function(i) prior.var(priors[i,])),
#                                row.names = rownames(priors))
  
  
  taupriors <- list(tauA = 0.01,
                    tauB = apply(prior.variances, 1, function(x) min(0.01, x)))
  
  
  ## run the meta-analysis
  trait.mcmc <- pecan.ma(trait.data, priors, taupriors,j.iter = ma_iter,settings,outdir)
  posteriors = approx.posterior(trait.mcmc,priors,trait.data,outdir)
  save(trait.mcmc, posteriors,file = paste(outdir, '/trait.mcmc.Rdata', sep=''))
       
  trait.stats <- sapply(trait.mcmc,function(x){summary(x)$statistics['beta.o',1:2]})
  pft.summary$mean[match(colnames(trait.stats),trait.name),i] = trait.stats[1,]
  pft.summary$sd[match(colnames(trait.stats),trait.name),i] = trait.stats[2,]
  
  outfile2 <- paste(outdir, '/pecan.MA.Rdata', sep = '')
  save.image(outfile2)
  ##save(outdir, file='outdir.Rdata')

  pecan.ma.summary(trait.mcmc, pft,outdir)
  
} ## end loop over pfts

save(pft.summary,file=paste(settings$outdir,"pft.summary.RData",sep=""))
