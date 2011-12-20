library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'ed'){
    settings.file = '~/pecan/fast.settings.xml'
  } else if(user == 'mantoot2'){
    settings.file = '/home/mantoot2/pecan/ebifarm.acru.xml'
  } else if(user == 'dlebauer'){
    settings.file = '~/in/ebifarm/prior/settings.pavi.xml'
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
require(PECAn)

trait.names <- trait.dictionary()$id
file.remove(dir(settings$outdir, full.names=TRUE))
## connect to database
newcon <- query.bety.con(dbname   = settings$database$name,
                                    password = settings$database$passwd,
                                    username = settings$database$userid,
                                    host     = settings$database$host)
cnt = 0;
all.trait.data = list()
for(pft in settings$pfts){
  cnt = cnt + 1
  
  ## 1. get species list based on pft
  spstr <- query.bety.pft_species(pft$name,con=newcon)
  
  ## 2. get priors available for pft  
  prior.distns <- query.bety.priors(pft$name, vecpaste(trait.names), out=pft$outdir,con=newcon)
  ### exclude any parameters for which a constant is provided 
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in%
                                     names(settings$pfts$pft$constants)),]
  
  print('Summary of Prior distributions')
  print(format(prior.distns, scientific = FALSE))
  traits <- rownames(prior.distns) # vector of variables with prior distributions for pft 

  ## if meta-analysis to be run, get traits for pft as a list with one dataframe per variable
  if('meta.analysis' %in% names(settings)) {
    trait.data <- query.bety.traits(spstr, traits, con = newcon)
    traits <- names(trait.data)
    save(trait.data, file = paste(pft$outdir, 'trait.data.Rdata', sep=''))
    
    all.trait.data[[cnt]] <- trait.data
    names(all.trait.data)[cnt] <- pft$name

    for(i in 1:length(all.trait.data)){
      writeLines(paste('pft:', pft$name,
                       '\n number of observations found for each trait'))
      print(as.list(sapply(all.trait.data[[i]], dim)[1, ]))
    }
    
  }
  save(prior.distns, file=paste(pft$outdir, 'prior.distns.Rdata', sep = ''))
}

