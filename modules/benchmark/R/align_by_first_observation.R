#################################################################
#'
#' align_first_observation
#' @param observation_one a vector of plant fucntional types, or species. Provides species/pft names.
#' @param observation_two another vector of plant fucntional types, or species. Provides the order. 
#' @param custom_table a table that either maps two pft's to one another or maps custom species codes to bety id codes. 
#' In the second case, must be passable to match_species_id. 
#' @return \code{vector} Returns a vector of PFT's/species from observation_one that matches the order of observation_two  
#' 
#' @author Tempest McCabe
#' @examples
#' 
#' observation_one<-c("AMCA3","AMCA3","AMCA3","AMCA3")
#' observation_two<-c("a", "b", "a", "a")
#' 
#' table<-list()
#' table$plant_functional_type_one<- c("AMCA3","AMCA3","ARHY", "ARHY")
#' table$plant_functional_type_two<- c('a','a','b', 'b') # PFT groupings
#' table<-as.data.frame(table)
#'
#' aligned <- align_by_first_observation(
#'   observation_one = observation_one,
#'   observation_two = observation_two,
#'   custom_table = table)
#'
#' # aligned should be a vector '[1] "AMCA3" "ARHY"  "AMCA3" "AMCA3"'
#' @export
align_by_first_observation<-function(observation_one, observation_two, custom_table){
  
  final<-c()
  
  for( i in seq_along(observation_two)){  # For loop finds "coursest" PFT. 
    
    subset<-custom_table[custom_table$plant_functional_type_two == observation_two[i],]
    
    if(length(subset$plant_functional_type_one) > length(subset$plant_functional_type_two)){
      
      final[i]<-as.character(subset$plant_functional_type_two)
      
    }else if(length(subset$plant_functional_type_one) < length(subset$plant_functional_type_two)){
      
      final[i]<-as.character(subset$plant_functional_type_one)
      
    }else if (length(subset$plant_functional_type_one) == length(subset$plant_functional_type_two)){
      
      final[i]<-as.character(subset$plant_functional_type_one)
      
    }else{
      
      PEcAn.logger::logger.warn("There are no subsets of the custom_table that are alignable. Likely a problem with the custom_table format")
    
      }
    
  }
  return(final)
}


