##' Duplicate existing prior for new pft
##'
##' Creates a new pft that is a duplicate of an existing pft,
##' including relationships with priors and species of the existing pft
##' @title Duplicate PFT
##' @param parent.pft.name 
##' @param new.pft.name 
##' @param new.pft.definition 
##' @return nothing, creates new pft in database as a side-effect
##' @author David LeBauer
##' @exampe \dontrun{
##' priordupe(parent.pft.name    = "tempdecid",
##'           new.pft.name       = "mytempdecid",
##'           new.pft.definition = "mytempdecid is a new pft")
##' }
priordupe <- function(parent.pft.name = NULL,
                      new.pft.name   = NULL,
                      new.pft.definition =  NULL){
                      
  require(PEcAn.DB)

  parent.pft.id <- query.base(paste("select id from pfts where name = ",
                                    parent.pft.name, ";"))

  ## create new pft
  query.base(paste("insert into pfts set definition = ",
                   newpftdefn, " name = ",
                   new.pft.name, ";"))
  new.pft.id <- query.base(paste("select id from pfts where name =",
                                 new.pft.name,";"))

  old.species.id <- query.base(paste("select specie_id from pfts_species where pft_id =",
                                     parent.pft.id, ";"))
  new.pfts_species <- c(pft_id = new.pft.id, specie_id = unique(old.species.id))
  
  query.base(paste("insert into pfts_species set pft_id = ",
                   new.pfts_species$pft_id,
                   "specie_id = ",
                   new.pfts_species$specie_id, ";"))

  old.priors <-  query.base(paste("select prior_id from pfts_priors where pft_id =",
                                  parent.pft.id, ";"))
  new.pfts_priors <- c(pft_id = new.pft.id,
                       prior_id = unique(old.priors))
  query.base(paste("insert into pfts_priors set pft_id = ",
                   new.pfts_priors$pft_id,
                   "specie_id = ",
                   new.pfts_priors$priors_id, ";"))
}
