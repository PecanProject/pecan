#'@title get_species_list_standard
#' Checks if custom_table:
#' 1. is formated correctly
#' 2. is complete (has all of the species/pft's in both observations)
#' 3. is condense-able (Could be represented as a hierachry)
#' 
#' @param observation_one a vector of plant fucntional types, or species
#' @param observation_two anouther vector of plant fucntional types, or species
#' @param custom_table a table that either maps two pft's to one anouther or maps custom species codes to bety id codes. 
#' In the second case, must be passable to match_species_id. 
#' @return \code{character} Returns "usda", "latin_name", "fia" or "custom"
#' @author Tempest McCabe
get_species_list_standard<-function(vars){
  
  if("species_id" %in% vars){
    return("usda")
  }else if("species_name" %in% vars){
    return('latin_name')
  }else if("species_USDA_symbol" %in% vars){
    return("usda")
  }else if("species_FIA_symbol" %in% vars){
    return('fia')
  }else if(!is.null(custom_table)){
    if("bety_species_id" %in% names(custom_table)){
      return("custom")
    }else{
      logger.warn("Note: custom_table does not have column named 'bety_species_id' and cannot be used with match_species_id(). This prohibits species-level mapping, but allows PFT level mapping.")
    }
  }else{
    return(FALSE)
  }
}
