#################################################################
#' @title align_by_observation_two
#' @param observation_one a vector of plant fucntional types, or species
#' @param observation_two anouther vector of plant fucntional types, or species
#' @param custom_table a table that either maps two pft's to one anouther or maps custom species codes to bety id codes. 
#' In the second case, must be passable to match_species_id. 
#' @return \code{vector} Returns a vector of PFT's/species from observation_two that matches the order of observation_one } 
#' @author Tempest McCabe
#' @examples
#' 
#' observation_one<-c("AMCA3","AMCA3","AMCA3","AMCA3")
#' observation_two<-c("a", "b", "a", "a")
#' table<-list()
#' table$plant_functional_type_one<- c("AMCA3","AMCA3","ARHY", "ARHY")
#' table$plant_functional_type_two<- c('a','a','b', 'b') # PFT groupings
#' table$input_code<-c("AMCA3","AMCA3","ARHY", "ARHY") # Species
#' table<-as.data.frame(table)
#'
#' format_one<-"species_USDA_symbol"
#' format_two<-"plant_funtional_type"
#' 
#' aligned<-align_by_observation_one(observation_one = observation_one, observation_two = observation_two, 
#' custom_table = table)
#' # aligned should be a vector '[1] "a" "a" "a" "a"'
#' @export

align_by_observation_two<-function(observation_one, observation_two, custom_table){
final<-c()
  for( i in 1:length(observation_two)){  # For loop finds "coursest" PFT. 
    subset<-custom_table[custom_table$plant_functional_type_one == observation_one[i],]
    if(length(subset$plant_functional_type_two)>length(subset$plant_functional_type_one)){
      final[i]<-as.character(subset$plant_functional_type_one)
    }else if(length(subset$plant_functional_type_two)>length(subset$plant_functional_type_one)){
      final[i]<-as.character(subset$plant_functional_type_two)
    }else if (length(subset$plant_functional_type_two)==length(subset$plant_functional_type_one)){
      final[i]<-as.character(subset$plant_functional_type_two)
    }else{
      PEcAn.logger::logger.warn("There are no subsets of the custom_table that are alignable. Likely a problem with the custom_table format")
      aligned_species_list$final<-NULL
    }
  }
  as.vector(final)
  return(final)
}

