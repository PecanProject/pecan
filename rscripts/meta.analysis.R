library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'ed'){
    settings.file = '~/pecan/fast.settings.xml'
  } else if(user == 'mantoot2'){
    settings.file = '~/pecan/ebifarm.acsa3.xml'
  } else if(user == 'dlebauer'){
    settings.file = '~/in/ebifarm/fast/ebifarm.pavi.xml'
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
if('meta.analysis' %in% names(settings)) {

  if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
  require(PECAn)
  
  ma.iter   = as.numeric(settings$meta.analysis$iter)
  
  ## identify pfts
  pft.names <- unlist(xpathApply(settings.xml, '//pfts//pft//name', xmlValue))
  
  if(length(settings$pfts) < 1) stop('no PFT specified')
  
  ## loop over pfts
  for(pft in settings$pfts){
    load(paste(pft$outdir, 'trait.data.Rdata', sep=''))
    load(paste(pft$outdir, 'prior.distns.Rdata', sep=''))
    trait.average <- sapply(trait.data,function(x){mean(x$Y,na.rm=TRUE)})
    
    ## Set gamma distribution prior
    prior.variances = as.data.frame(rep(1,nrow(prior.distns)))
    row.names(prior.variances) = row.names(prior.distns)
    prior.variances[names(trait.average),] = 0.001*trait.average^2 
    prior.variances["seedling_mortality",1] = 1.0
    taupriors <- list(tauA = 0.01,
                      tauB = apply(prior.variances, 1, function(x) min(0.01, x)))
    
    ## run the meta-analysis
    trait.mcmc  <- pecan.ma(trait.data, prior.distns, taupriors, j.iter = ma.iter, settings, pft$outdir)
    post.distns <- approx.posterior(trait.mcmc,prior.distns,trait.data,pft$outdir)

    pecan.ma.summary(trait.mcmc, pft$name, pft$outdir)

    save(trait.mcmc, file = paste(pft$outdir, 'trait.mcmc.Rdata', sep=''))
    save(post.distns, file = paste(pft$outdir, 'post.distns.Rdata', sep = ''))  

  } ## end loop over pfts
} else {
  print('settings file does not call for meta-analysis')
}
