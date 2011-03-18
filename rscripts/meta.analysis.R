library(XML)
if(interactive()){
  settings.file <- '~/pecan/settings.pavi.xml'
} else {
  settings.file <- system("echo $PECANSETTINGS", intern = TRUE)
}

settings.xml <- xmlTreeParse(settings.file)
settings <- xmlToList(settings.xml)

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
ITER   <- as.numeric(settings$meta.analysis$iter)
M      <- as.numeric(settings$ensemble$size)
outdir <- settings$outdir
## trstr is a list of the traits that ED can use


require(PECAn)

trait.names <- c('mort2','cuticular_cond','dark_respiration_factor','plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width','nonlocal_dispersal','q','root_respiration_factor','root_turnover_rate','seedling_mortality','SLA','stomatal_slope','Vm_low_temp','quantum_efficiency','f_labile','c2n_leaf','water_conductance','Vcmax','r_fract','storage_turnover_rate','agf_bs')
trstr <- vecpaste(trait.names)
 
n.trait = length(trait.names)

ma.iter   = as.numeric(settings$meta.analysis$iter)
ensemble.size = as.numeric(settings$ensemble$size)
sensitivity.analysis = !is.null(settings$sensitivity.analysis)



## connect to database
con <- settings$database
if(settings$database$location == 'localhost'){
  con <- query.bety.con(dbname=con$name,password=con$passwd,username=con$userid)
}

## identify pfts
pft.name <- sapply(settings['pfts']$pft, function(x)x$name)
npft   <- length(pft.name)
if(npft < 1 | is.null(npft)) stop('no PFT specified')
mtemp <- matrix(NA,n.trait,npft)
row.names(mtemp) <- trait.names
colnames(mtemp) <- pft.name
pft.summary <- list(mean = mtemp,sd=mtemp,n=mtemp)

### loop over pfts
for( i in 1:length(pft.name)){

  pft    <- settings[['pfts']][[i]]$name
  outdir <- settings[['pfts']][[i]]$outdir
  
  ## 1. get species list based on pft
  spstr <- query.bety.pft_species(pft,con=con)
  
  ## 2. get priors available for pft  
  prior.data <- query.bety.priors(pft, trstr,out=outdir,con=con)
  print(prior.data)
  if(!is.null(settings[['pfts']][[i]]$priors)) {
    prior.data <- prior.data[which(rownames(prior.data) %in% settings[[pfts[i]]]$priors),]
  }
  priors <- rownames(prior.data) # vector of variables with prior distributions for pft 
  prior.defs <- trait.dictionary(priors)
  save(prior.defs, file = paste(outdir, '/prior.defs.Rdata', sep=''))
  
  ## get traits for pft as a list with one dataframe per variable
  trait.data <- query.bety.traits(spstr,priors,con=con)
  traits <- names(trait.data)

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
  

  ##prior.variances <- data.frame(var = unlist(t(sapply(1:nrow(prior.data), function(i) with(prior.data[i,], pdf.stats(distn, parama, paramb)))['var',])), row.names = priors)
  prior.variances = as.data.frame(rep(1,nrow(prior.data)))
  row.names(prior.variances) = row.names(prior.data)
  prior.variances[names(trait.average),] = 0.001*trait.average^2 

  ## Set gamma distribution prior on
#  prior.var <- function(x) do.call(pdf.stats, list(x$distn, x$parama, x$paramb))['var']
#  prior.variances <- data.frame(var = sapply(1:nrow(prior.data), function(i) prior.var(prior.data[i,])),
#                                row.names = priors)
  
  
  taupriors <- list(tauA = 0.01,
                    tauB = apply(prior.variances, 1, function(x) min(0.01, x)))
  
  
  ## run the meta-analysis
  trait.mcmc <- pecan.ma(trait.data, prior.data, taupriors, j.iter = ma.iter, settings, outdir)
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
