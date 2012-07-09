##--------------------------------------------------------------------------------------------------#
##' 
##' @name get.trait.data
##' @title Gets trait data from the database
##'
##'
##' @import PEcAn.utils
##' @author David LeBauer, Shawn Serbin
##' @export
##'
get.trait.data <- function() {
  ## Info:  lots of hacks for now.  Needs to be updated once full workflow is ready.
  num <- sum(names(unlist(settings$pfts)) == "pft.name")
  for (i in 1:num){
    out.dir <- settings$pfts[i]$pft$outdir
    if (!file.exists(out.dir)) {
      dir.create(out.dir, recursive=TRUE)
    }
    ## Remove old files.  Clean up.
    old.files <- list.files(path=settings$pfts[i]$pft$outdir,
                            full.names=TRUE, pattern = "*.Rdata") 
    file.remove(old.files[which(file.info(list.files(path=settings$pfts[i]$pft$outdir,
                                                     full.names=TRUE))$isdir==FALSE)])
    
    rm(out.dir)
  }
  ##--------------------------------------------------------------------------------------------------#


  ##---------------- Load trait dictionary. ----------------------------------------------------------#
  ed.trait.dictionary <- read.csv(system.file("data", "ed.trait.dictionary.csv",
                                              package = "PEcAn.utils"), sep = ";")
  trait.names <- ed.trait.dictionary$id
  ##--------------------------------------------------------------------------------------------------#


  ##---------------- Open database connection. -------------------------------------------------------#
  
  newconfn <- function() query.base.con(settings)

  ##--------------------------------------------------------------------------------------------------#


  ##---------------- Query trait data. ---------------------------------------------------------------#
  cnt = 0;
  all.trait.data = list()
  for(pft in settings$pfts){
    out.dir = pft$outdir # loop over pfts
    
    cnt = cnt + 1
    
    ## 1. get species list based on pft
    spstr <- query.pft_species(pft$name,con=newconfn())
    
    ## 2. get priors available for pft  
    prior.distns <- query.priors(pft$name, vecpaste(trait.names),
                                 out=pft$outdir,con=newconfn())
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
      trait.data <- query.traits(spstr, traits, con = newconfn())
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
}
##==================================================================================================#


####################################################################################################
### EOF.  End of R script file.    					
####################################################################################################
