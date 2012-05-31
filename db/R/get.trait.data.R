#--------------------------------------------------------------------------------------------------#
##' 
##'
##'
##'
##'
##'
##'
##'
##'
#==================================================================================================#
# Info:  lots of hacks for now.  Needs to be updated once full workflow is ready.


#---------------- Load requirements for function. -------------------------------------------------#
# Info: Current set as hacks.  Need to replace once the packages compile

setwd('/home/sserbin/pecan_trunk/')

#ok = require(db); if (! ok) stop("Package db is not available...")      # R XML library
source('./db/R/query.base.R')
source('./db/R/query.pft.R')
source('./db/R/query.prior.R')
source('./db/R/query.trait.data.R')
source('./db/R/query.traits.R')

# Utils
source('./utils/R/utils.R')
source('./modules/meta.analysis/R/jagify.R')


# HACK: Just a hack for now.
source('./common/R/read.settings.R')

ok = require(RMySQL); if (! ok) stop("Package RMySQL is not available...")      # R MySQL library
ok = require(rjags); if (! ok) stop("Package rjags is not available...")      # R MySQL library
ok = require(plyr); if (! ok) stop("Package plyr is not available...")      # R MySQL library
#--------------------------------------------------------------------------------------------------#


#---------------- Load PEcAn settings file. -------------------------------------------------------#
default.settings <- Sys.getenv(x = c("SETTINGS","USER","HOME"))   # Import default location
args <- commandArgs(trailingOnly = TRUE)                          # Import command argument

if (is.na(args[1])==TRUE){
  pecan.settings.file <- default.settings[1]
  print(paste("---- PEcAn settings file: ",pecan.settings.file, sep = ""))
  print(" ")
} else {
  pecan.settings.file <- args[1]
  print(paste("---- PEcAn settings file: ",pecan.settings.file, sep = ""))
  print(" ")
}

# Open and read in settings file for PEcAn run.
settings = read.settings(pecan.settings.file)
#--------------------------------------------------------------------------------------------------#


#---------------- Clean up output directory. ------------------------------------------------------#
# Info: May not want to keep all of this code block?
#file.remove(list.files(path=settings$outdir,full.names=TRUE)
#            [which(file.info(list.files(path=settings$outdir,full.names=TRUE))$isdir==FALSE)])

# Create output directories if they don't already exist.  Clean up old files.
num = length(settings$pfts)
for (i in 1:num){
  out.dir = settings$pfts[i]$pft$outdir
  if (! file.exists(out.dir)) dir.create(out.dir)
  
  # Remove old files.  Clean up.
  file.remove(list.files(path=settings$pfts[i]$pft$outdir,full.names=TRUE)
              [which(file.info(list.files(path=settings$pfts[i]$pft$outdir,
                                          full.names=TRUE))$isdir==FALSE)])
  
  rm(out.dir)
}
#--------------------------------------------------------------------------------------------------#


#---------------- Load trait dictionary. ----------------------------------------------------------#
# Info:  A hack. need to remove this dependency.
trait.names <- trait.dictionary()$id
#--------------------------------------------------------------------------------------------------#


#---------------- Open database connection. -------------------------------------------------------#
newconfn <- function() query.base.con(dbname   = settings$database$name,
                                      password = settings$database$passwd,
                                      username = settings$database$userid,
                                      host     = settings$database$host)

newcon <- newconfn()
#--------------------------------------------------------------------------------------------------#


#---------------- Query trait data. ---------------------------------------------------------------#
cnt = 0;
all.trait.data = list()
for(pft in settings$pfts){
  out.dir = pft$outdir
  if (! file.exists(out.dir)) dir.create(out.dir)
  #dir.create(pft$outdir)  # keep this here or do this above?
  cnt = cnt + 1
  
  ## 1. get species list based on pft
  spstr <- query.pft_species(pft$name,con=newcon)
  
  ## 2. get priors available for pft  
  prior.distns <- query.priors(pft$name, vecpaste(trait.names), out=pft$outdir,con=newcon)
  ### exclude any parameters for which a constant is provided 
  prior.distns <- prior.distns[which(!rownames(prior.distns) %in%
    names(pft$constants)),]
  
  # 3. display info to the console
  print(" ")
  print("-------------------------------------------------------------------")
  print(paste('Summary of Prior distributions for: ',pft$name,sep=""))
  print(prior.distns)
  traits <- rownames(prior.distns) # vector of variables with prior distributions for pft 
  print("-------------------------------------------------------------------")
  print(" ")
  
  ## if meta-analysis to be run, get traits for pft as a list with one dataframe per variable
  if('meta.analysis' %in% names(settings)) {
    trait.data <- query.traits(spstr, traits, con = newcon)
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


####################################################################################################
### EOF.  End of R script file.    					
####################################################################################################