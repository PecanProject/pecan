#' check_if_species_list
#'
#'@details
#' Checks if format contains a species list in a known format, or a declared custom format. 
#'
#' @param vars format
#' @param custom_table a table that either maps two pft's to one anouther or maps custom species codes to bety id codes. 
#' In the second case, must be passable to match_species_id. 
#'
#' @return \code{boolean} 
#' @author Tempest McCabe
check_if_species_list<-function(vars,custom_table=NULL){
  
  if(any(c("species_id", "species_name", "species_USDA_symbol", "species_FIA_symbol") %in% vars)){
    return(TRUE)
  }else if(!is.null(custom_table)){
    if("bety_species_id" %in% names(custom_table)){
      return(TRUE)
    }else{
      PEcAn.logger::logger.warn("Note: custom_table does not have column named 'bety_species_id' and cannot be used with match_species_id().  
                  Tables that do not have a 'bety_species_id' column cannot be used for species-level mapping,
                  but can be used for PFT level mapping.")
    }
  }else{
    return(FALSE)
  }
}
  

