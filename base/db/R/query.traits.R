#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#------------------------------------------------------------------------------#
##' Query available trait data associated with a given pft and a list of traits
##'
##' @name query.traits
##' @title Query trait data
##' @param ids vector of species or cultivar id's from trait database
##' @param priors vector of parameters for which priors have been specified
##' @param con database connection object
##' @param update.check.only if TRUE, returns results but does not print summaries
##' @param ids_are_cultivars if TRUE, ids is a vector of cultivar IDs, otherwise they are species IDs
##' @return list of dataframes, each with data for one trait
##' @seealso \code{\link{query.trait.data}}
##' @export query.traits
##' @examples
##' \dontrun{
##' con <- db.open(your_settings_here)
##' species <- query.pft_species('ebifarm.c4crop')
##' spstr <- vecpaste(species$id)
##' trvec <- c('leafN', 'SLA')
##' trait.data <- query.traits(spstr, trvec, con)
##' }
##' @author David LeBauer, Carl Davidson, Shawn Serbin
query.traits <- function(ids, priors, con,
                         update.check.only = FALSE,
                         ids_are_cultivars = FALSE) {


  if (!inherits(con, "DBIConnection")) {
    PEcAn.logger::logger.severe("'con' is not a database connection")
  }

  if (length(ids) == 0 || length(priors) == 0) {
    return(list())
  }
  
  id_type = rlang::sym(if (ids_are_cultivars) {"cultivar_id"} else {"specie_id"})
  
  traits <- (dplyr::tbl(con, "traits")
             %>% dplyr::inner_join(dplyr::tbl(con, "variables"), by = c("variable_id" = "id"))
             %>% dplyr::filter(
               (!!id_type %in% ids),
               (name %in% !!priors)) # TODO: use .data$name when filter supports it
             %>% dplyr::distinct(name) # TODO: use .data$name when distinct supports it
             %>% dplyr::collect())
  
  if (nrow(traits) == 0) {
    return(list())
  }
  
  ### Grab trait data
  trait.data <- lapply(traits$name, function(trait){
    query.trait.data(
      trait = trait,
      spstr = PEcAn.utils::vecpaste(ids),
      con = con,
      update.check.only = update.check.only,
      ids_are_cultivars = ids_are_cultivars)
  })
  names(trait.data) <- traits$name
  
  return(trait.data)
}