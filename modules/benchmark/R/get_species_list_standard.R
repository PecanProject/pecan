#' get_species_list_standard
#'
#' Returns the format type for convience of use with match_species_id
#'
#' @param vars format to be matched
#' @return \code{character} Returns "usda", "latin_name", "fia" or "custom"
#' @author Tempest McCabe
get_species_list_standard <- function(vars) {
  
  if(any(c("species_id", "species_USDA_symbol") %in% vars)){
    return("usda")
  }else if("species_name" %in% vars){
    return('latin_name')
  }else if("species_FIA_symbol" %in% vars){
    return('fia')
  }else if(!is.null(vars)){
    if("bety_species_id" %in% names(vars)){
      return("custom")
    }else{
      PEcAn.logger::logger.warn("Note: `vars` does not have column named 'bety_species_id' and cannot be used with match_species_id(). This prohibits species-level mapping, but allows PFT level mapping.")
    }
  }else{
    return(FALSE)
  }
}
