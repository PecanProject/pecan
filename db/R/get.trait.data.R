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

  num <- sum(names(unlist(settings$pfts)) == "pft.name")
  for (i in 1:num){
    ## Remove old files.  Clean up.
    old.files <- list.files(path=settings$pfts[i]$pft$outdir,
                            full.names=TRUE)
    file.remove(old.files[which(file.info(list.files(path=settings$pfts[i]$pft$outdir,
                                                     full.names=TRUE))$isdir==FALSE)])
  }
  ##---------------- Load trait dictionary --------------#

  data(trait.dictionary, package = "PEcAn.utils")
  trait.names <- trait.dictionary$id
  ##---------------- Query trait data --------------------#

  all.trait.data <- list()
  dbcon <- db.open(settings$database)
  for(pft in settings$pfts){
 
      ## 1. get species list based on pft
      species <- query.pft_species(pft$name, con=dbcon)
      spstr <- vecpaste(species$id)
      write.csv(species, file.path(pft$outdir, "species.csv"), row.names = FALSE)
      ## 2. get priors available for pft  
      prior.distns <- query.priors(pft$name, vecpaste(trait.names),
                                   out = pft$outdir, con = dbcon)
      
      ## exclude any parameters for which a constant is provided 
      prior.distns <- prior.distns[which(!rownames(prior.distns) %in%
                                         names(pft$constants)),]

      ## save priors
      save(prior.distns, file = file.path(pft$outdir, "prior.distns.Rdata"))
      write.csv(prior.distns,
                file = file.path(pft$outdir, "prior.distns.csv"), row.names = FALSE)
    
      ## 3. display info to the console
      logger.info('Summary of Prior distributions for: ', pft$name)
      logger.info(prior.distns)

      ## traits = variables with prior distributions for this pft 
      traits <- rownames(prior.distns) 
      
      trait.data <- query.traits(spstr, traits, con = dbcon)
      traits <- names(trait.data)
      trait.data.file <- file.path(pft$outdir, "trait.data.Rdata")
      save(trait.data, file = trait.data.file)
      write.csv(ldply(trait.data),
                file = file.path(pft$outdir, "trait.data.csv"), row.names = FALSE)
      if(!file.exists(trait.data.file)){
          logger.error("trait.data not saved")
      }
      
      all.trait.data[[pft$name]] <- trait.data
      
      for(i in 1:length(all.trait.data)){
          logger.info("number of observations per trait for", pft$name)
          logger.info(ldply(all.trait.data[[i]], nrow))
      }
      
  }
  db.close(dbcon)
}
##==================================================================================================#


####################################################################################################
### EOF.  End of R script file.    					
####################################################################################################
