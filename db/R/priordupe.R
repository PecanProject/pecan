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
##' @examples \dontrun{
##' priordupe(parent.pft.name    = "tempdecid",
##'           new.pft.name       = "mytempdecid",
##'           new.pft.definition = "mytempdecid is a new pft")
##' }
priordupe <- function(parent.pft.name = NULL,
                      new.pft.name   = NULL,
                      new.pft.definition =  NULL,
                      settings = NULL){
                      
  require(PEcAn.DB)
  con <- db.open(settings$database$bety)
  parent.pft.id <- db.query(paste("select id from pfts where name = ",
                                    parent.pft.name, ";"), con=con)

  ## create new pft
  db.query(paste("insert into pfts set definition = ",
                   newpftdefn, " name = ",
                   new.pft.name, ";"), con=con)
  new.pft.id <- db.query(paste("select id from pfts where name =",
                                 new.pft.name,";"), con=con)

  old.species.id <- db.query(paste("select specie_id from pfts_species where pft_id =",
                                     parent.pft.id, ";"), con=con)
  new.pfts_species <- c(pft_id = new.pft.id, specie_id = unique(old.species.id))
  
  db.query(paste("insert into pfts_species set pft_id = ",
                   new.pfts_species$pft_id,
                   "specie_id = ",
                   new.pfts_species$specie_id, ";"), con=con)

  old.priors <-  db.query(paste("select prior_id from pfts_priors where pft_id =",
                                  parent.pft.id, ";"), con=con)
  new.pfts_priors <- c(pft_id = new.pft.id,
                       prior_id = unique(old.priors))
  db.query(paste("insert into pfts_priors set pft_id = ",
                   new.pfts_priors$pft_id,
                   "specie_id = ",
                   new.pfts_priors$priors_id, ";"), con=con)
  db.close(con)
}
