#' check_if_list_of_pfts
#'
#' Checks if format contains a variable named "plant_functional_type"
#'
#' @param vars names to check
#'
#' @return \code{boolean} 
#' @author Tempest McCabe
check_if_list_of_pfts<-function(vars){
  
  if( any(c("plant_functional_type","species_name") %in% vars)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
  
