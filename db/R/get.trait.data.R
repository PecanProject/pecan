#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------#
##' 
##' @name get.trait.data
##' @title Gets trait data from the database
##' @author David LeBauer, Shawn Serbin
##' @export
##'
get.trait.data <- function() {
  ## Info:  lots of hacks for now.  Needs to be updated once full workflow is ready.
  require(RMySQL)
  require(PEcAn.utils)
  num <- sum(names(unlist(settings$pfts)) == "pft.name")
  for (i in 1:num){
    ## Remove old files.  Clean up.
    old.files <- list.files(path=settings$pfts[i]$pft$outdir,
                            full.names=TRUE)
    file.remove(old.files[which(file.info(list.files(path=settings$pfts[i]$pft$outdir,
                                                     full.names=TRUE))$isdir==FALSE)])
  }
  ##--------------------------------------------------------------------------------------------------#


  ##---------------- Load trait dictionary. ----------------------------------------------------------#

  data(trait.dictionary, package = "PEcAn.utils")
  trait.names <- trait.dictionary$id
  ##--------------------------------------------------------------------------------------------------#


  ##--------------------------------------------------------------------------------------------------#


  ##---------------- Query trait data. ---------------------------------------------------------------#

  all.trait.data <- list()
  dbcon <- db.open(settings$database)
  for(pft in settings$pfts){
 
    ## 1. get species list based on pft
    spstr <- query.pft_species(pft$name, con=dbcon)
    
    ## 2. get priors available for pft  
    prior.distns <- query.priors(pft$name, vecpaste(trait.names),
                                 out = pft$outdir, con = dbcon)
    
    ### exclude any parameters for which a constant is provided 
    prior.distns <- prior.distns[which(!rownames(prior.distns) %in%
                                       names(pft$constants)),]

    ### save priors
    save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))

    
    # 3. display info to the console
    print(" ")
    print("-------------------------------------------------------------------")
    print(paste('Summary of Prior distributions for: ',pft$name,sep=""))
    print(prior.distns)
    traits <- rownames(prior.distns) # vector of variables with prior distributions for pft 
    print("-------------------------------------------------------------------")
    print(" ")
    
    trait.data <- query.traits(spstr, traits, con = dbcon)
    traits <- names(trait.data)
    trait.data.file <- file.path(pft$outdir, "trait.data.Rdata")
    save(trait.data, file = trait.data.file)
    if(!file.exists(trait.data.file)){
      stop("trait.data not saved")
    }
    
    all.trait.data[[pft$name]] <- trait.data
    
    for(i in 1:length(all.trait.data)){
      print(paste("number of observations per trait for", pft$name))
      print(ldply(all.trait.data[[i]], nrow))
    }
    
  }
  db.close(dbcon)
}
##==================================================================================================#


####################################################################################################
### EOF.  End of R script file.    					
####################################################################################################
