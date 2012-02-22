library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user %in% c('pecan', 'dlebauer')){
    settings.file = '~/in/ebifarm/fast/pavi.xml'
  } else {
    paste('please specify settings file in meta.analysis.R')
  }
} else {
  settings.file <- Sys.getenv("PECANSETTINGS")
}
settings.xml <- xmlParse(settings.file)
settings <- xmlToList(settings.xml)
if(!is.null(settings$Rlib)){ .libPaths(settings$Rlib)} 
require(PECAn)

trait.names <- trait.dictionary()$id
file.remove(dir(settings$outdir, full.names=TRUE))
## connect to database
newconfn <- function() query.bety.con(dbname   = settings$database$name,
                                      password = settings$database$passwd,
                                      username = settings$database$userid,
                                      host     = settings$database$host)

newcon <- newconfn()

cnt = 0;
all.trait.data = list()
for(pft in settings$pfts){
  dir.create(pft$outdir)
  cnt = cnt + 1
  
  ## 1. get species list based on pft
  spstr <- query.bety.pft_species(pft$name,con=newcon)
  
  ## 2. get priors available for pft  
  prior.distns <- query.bety.priors(pft$name, vecpaste(trait.names), out=pft$outdir,con=newcon)
  ### exclude any parameters for which a constant is provided 
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in%
                                     names(pft$constants)),]
  
  print('Summary of Prior distributions')
  print(prior.distns)
  traits <- rownames(prior.distns) # vector of variables with prior distributions for pft 

  ## if meta-analysis to be run, get traits for pft as a list with one dataframe per variable
  if('meta.analysis' %in% names(settings)) {
    trait.data <- query.bety.traits(spstr, traits, con = newcon)
    traits <- names(trait.data)
    save(trait.data, file = paste(pft$outdir, 'trait.data.Rdata', sep=''))
    
    all.trait.data[[cnt]] <- trait.data
    names(all.trait.data)[cnt] <- pft$name

    for(i in 1:length(all.trait.data)){
      print(names(all.trait.data)[i])
      print(sapply(all.trait.data[[i]],dim))
    }
    
  }
  save(prior.distns, file=paste(pft$outdir, 'prior.distns.Rdata', sep = ''))
}
