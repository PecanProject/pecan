#' get_species_list_standard
#' @details 
#' Returns the format type for convience of use with match_species_id
#' @param observation_one a vector of plant fucntional types, or species
#' @param observation_two anouther vector of plant fucntional types, or species
#' @param custom_table a table that either maps two pft's to one another or maps custom species codes to bety id codes. 
#' In the second case, must be passable to match_species_id. 
#' @return \code{character} Returns "usda", "latin_name", "fia" or "custom"
#' @author Tempest McCabe
get_species_list_standard<-function(vars){
  
  if(any(c("species_id", "species_USDA_symbol") %in% vars)){
    return("usda")
  }else if("species_name" %in% vars){
    return('latin_name')
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
