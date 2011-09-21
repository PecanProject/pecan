library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'ed'){
    settings.file = '~/pecan/fast.settings.xml'
  } else if(user == 'mantoot2'){
    settings.file = '~/pecan/ebifarm.acsa3.xml'
  } else if(user == 'dlebauer'){
    settings.file = '~/pecan/settings.ebifarm.pavi.xml'
#    settings.file = '~/pecan/ebifarm.acsa3.xml'
  } else if(user == 'davids14') {
    settings.file = '~/pecan/tundra.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
}

settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)

if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
ITER   <- as.numeric(settings$meta.analysis$iter)
M      <- as.numeric(settings$ensemble$size)
outdir <- settings$outdir
## trstr is a list of the traits that ED can use


require(PECAn)

trait.names <- c('mort2','cuticular_cond','dark_respiration_factor',
    'plant_min_temp','growth_resp_factor','leaf_turnover_rate','leaf_width',
    'nonlocal_dispersal','fineroot2leaf','root_respiration_rate',
    'root_turnover_rate','seedling_mortality','SLA','stomatal_slope',
    'Vm_low_temp','quantum_efficiency','f_labile','c2n_leaf',
    'water_conductance','r_fract','storage_turnover_rate','agf_bs','Vcmax',
    'b1Ht', 'b2Ht', 'b1Bl', 'b2Bl', 'b1Bs', 'b2Bs')

trstr <- vecpaste(trait.names)
 
n.trait = length(trait.names)

ma.iter   = as.numeric(settings$meta.analysis$iter)
ensemble.size = as.numeric(settings$ensemble$size)
sensitivity.analysis = !is.null(settings$sensitivity.analysis)

## connect to database
con <- query.bety.con(dbname   = settings$database$name,
                      password = settings$database$passwd,
                      username = settings$database$userid,
                      host     = settings$database$host)

## identify pfts
pfts <- settings$pfts
pft.names <- unlist(xpathApply(settings.xml, '//pfts//pft//name', xmlValue))
npft   <- length(pfts)

if(npft < 1 | is.null(npft)) stop('no PFT specified')
mtemp <- matrix(NA,n.trait,npft)
row.names(mtemp) <- trait.names
colnames(mtemp) <- pft.names
pft.summary <- list(mean = mtemp,sd=mtemp,n=mtemp)

### loop over pfts
for( pft in pfts){
  
  ## 1. get species list based on pft
  spstr <- query.bety.pft_species(pft$name,con=con)
  
  ## 2. get priors available for pft  
  prior.distns <- query.bety.priors(pft$name, trstr,out=pft$outdir,con=con)
  ### exclude any parameters for which a constant is provided 
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in%
                                     names(pft$constants)),]
  print('Summary of Prior distributions')
  print(prior.distns)
  priors <- rownames(prior.distns) # vector of variables with prior distributions for pft 
  prior.defs <- trait.dictionary(priors)
  save(prior.defs, file = paste(pft$outdir, '/prior.defs.Rdata', sep=''))
  
  ## get traits for pft as a list with one dataframe per variable
  trait.data <- query.bety.traits(spstr,priors,con=con)
  traits <- names(trait.data)
  
  ## DATA HACKS **** THESE SHOULD BE FIXED IN THE DATABASE*******
  if("root_respiration_rate" %in% names(trait.data)){
    sel = which(trait.data[["root_respiration_rate"]]$Y < 0.05)
    trait.data[["root_respiration_rate"]]$Y[sel] = trait.data[["root_respiration_rate"]]$Y[sel]*1000
  }
  if("SLA" %in% names(trait.data)){
    sel = which(trait.data[["SLA"]]$cite %in% c(311))
    if(length(sel) > 0){
      trait.data[["SLA"]] = trait.data[["SLA"]][-sel,]            
    }
  }
  save(trait.data, file = paste(pft$outdir, '/trait.data.Rdata', sep=''))

  if(spstr != "''"){
    trait.count <- sapply(trait.data,nrow)
    trait.average <- sapply(trait.data,function(x){mean(x$Y,na.rm=TRUE)})

    pft.summary$n[match(names(trait.count),traits), pft$name] <- trait.count
    
    ##prior.variances <- data.frame(var = unlist(t(sapply(1:nrow(prior.distns), function(i) with(prior.distns[i,], pdf.stats(distn, parama, paramb)))['var',])), row.names = priors)
    prior.variances = as.data.frame(rep(1,nrow(prior.distns)))
    row.names(prior.variances) = row.names(prior.distns)
    prior.variances[names(trait.average),] = 0.001*trait.average^2 
    prior.variances["seedling_mortality",1] = 1.0
    
    ## Set gamma distribution prior on
    ##  prior.var <- function(x) do.call(pdf.stats, list(x$distn, x$parama, x$paramb))['var']
    ##  prior.variances <- data.frame(var = sapply(1:nrow(prior.distns), function(i) prior.var(prior.distns[i,])),
    ##                                row.names = priors)
  
  
    taupriors <- list(tauA = 0.01,
                      tauB = apply(prior.variances, 1, function(x) min(0.01, x)))
    ##NOTE: with leaf_width in units of mm instead of m, all parameters are on similar scale
    ##NOTE: could reinstate this, but I am not sure that we have the correct transformation here
    ##NOTE: for now, these will be fixed at 0.01
    ##tauB = apply(prior.variances, 1, function(x) min(0.01, 0.01*x)))
  
    ## run the meta-analysis
    trait.mcmc  <- pecan.ma(trait.data, prior.distns, taupriors, j.iter = ma.iter, settings, pft$outdir)
    post.distns <- approx.posterior(trait.mcmc,prior.distns,trait.data,pft$outdir)
    save(trait.mcmc, post.distns, file = paste(pft$outdir, '/trait.mcmc.Rdata', sep=''))
  
    
    ma.traitnames <- names(trait.mcmc)

    ## Is the following code useful? Perhaps we should plot this instead
    trait.stats <- sapply(trait.mcmc,function(x){summary(x)$statistics['beta.o',1:2]})
    pft.summary$mean[match(colnames(trait.stats), ma.traitnames),pft$name] <- trait.stats[1, ]
    pft.summary$sd[match(colnames(trait.stats), ma.traitnames),pft$name] <- trait.stats[2, ]
  
    pecan.ma.summary(trait.mcmc, pft$name, pft$outdir)
  }
  save(prior.distns, file=paste(pft$outdir, '/prior.distns.Rdata', sep = ''))
  save(post.distns, file=paste(pft$outdir, '/post.distns.Rdata', sep = ''))
    
} ## end loop over pfts

save(pft.summary,file=paste(settings$outdir,"pft.summary.Rdata",sep=""))
