#' check_if_list_of_pfts
#' @details Checks if format contains a variable named "plant_functional_type"
#' @param observation_one a vector of plant fucntional types, or species
#' @param observation_two anouther vector of plant fucntional types, or species
#' @param custom_table a table that either maps two pft's to one anouther or maps custom species codes to bety id codes. 
#' In the second case, must be passable to match_species_id. 
#' @return \code{boolean} 
#' @author Tempest McCabe
check_if_list_of_pfts<-function(vars){
  
  if( any(c("plant_functional_type","species_name") %in% vars)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
  
