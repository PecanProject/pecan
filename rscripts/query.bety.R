library(XML)
if(interactive()){
  user <- Sys.getenv('USER')
  if(user == 'ed'){
    settings.file = '~/pecan/fast.settings.xml'
  } else if(user == 'mantoot2'){
    settings.file = '/home/dlebauer/pecan/ebifarm.acru.xml'
  } else if(user == 'dlebauer'){
    settings.file = '~/in/ebifarm/post/settings.pavi.xml'
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

## connect to database
newcon <- function(){query.bety.con(dbname   = settings$database$name,
                                    password = settings$database$passwd,
                                    username = settings$database$userid,
                                    host     = settings$database$host)}

for(pft in settings$pfts){
  
  ## 1. get species list based on pft
  spstr <- query.bety.pft_species(pft$name,con=newcon())
  
  ## 2. get priors available for pft  
  prior.distns <- query.bety.priors(pft$name, vecpaste(trait.names), out=pft$outdir,con=newcon())
  ### exclude any parameters for which a constant is provided 
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in%
                                     names(settings$pfts$pft$constants)),]
  
  print('Summary of Prior distributions')
  print(prior.distns)
  traits <- rownames(prior.distns) # vector of variables with prior distributions for pft 

  ## if meta-analysis to be run, get traits for pft as a list with one dataframe per variable
  if('meta.analysis' %in% names(settings)) {
    trait.data <- query.bety.traits(spstr, traits, con = newcon())
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
  }
  save(prior.distns, file=paste(pft$outdir, '/prior.distns.Rdata', sep = ''))
}
