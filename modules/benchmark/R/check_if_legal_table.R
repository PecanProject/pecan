#' check_if_legal_table
#'
#' @details
#' Checks if custom_table:
#' 1. is formated correctly
#' 2. is complete (has all of the species/pft's in both observations)
#' 3. is condense-able (Could be represented as a hierachry)
#' 
#' @param table a table that either maps two pft's to one anouther or maps custom species codes to bety id codes.
#'   In the second case, must be passable to match_species_id.
#' @param observation_one a vector of plant functional types, or species
#' @param observation_two anouther vector of plant functional types, or species
#'
#' @return \code{boolean} 
#' @author Tempest McCabe
check_if_legal_table<-function(table, observation_one, observation_two){
  all_there<-TRUE
  names<-names(table)
  if(!"plant_functional_type_one" %in% names|!"plant_functional_type_two" %in% names ){
    PEcAn.logger::logger.severe("Custom table provided does not use correct column names. Requires both 'plant_functional_type_one', and 'plant_functional_type_two'. 
                  Column names are currently", names(table))
  }else{
    missing<-list()
    for(h in seq_along(observation_one)){
      if(!observation_one[h] %in% table$plant_functional_type_one){
        all_there<-FALSE; missing<-c(missing,observation_one[h])
        }
    }
    for(h in seq_along(observation_two)){
      if(!observation_two[h] %in% table$plant_functional_type_two){
        all_there<-FALSE; missing<-c(missing,observation_two[h])
        }
    }
    if(all_there){
      is_legal_table<-TRUE
      pft_1<-as.character(unique(table$plant_functional_type_one))
      pft_2<-as.character(unique(table$plant_functional_type_two))
      
      for(i in seq_along(pft_1)){
        aggregated_1<-FALSE
        aggregated_2<-FALSE
        
        subset<-subset(table, table$plant_functional_type_one == pft_1[i])
        
        length_of_pft_1_uniques_1<-length(as.character(unique(subset$plant_functional_type_one)))
        length_of_pft_2_uniques_1<-length(as.character(unique(subset$plant_functional_type_two)))
        
        if(length_of_pft_2_uniques_1>1 | length_of_pft_1_uniques_1>1){
          aggregated_1<- TRUE
          }
        
        for(j in seq_along(unique(subset$plant_functional_type_two))){
          
          subset_2<-subset(table, table$plant_functional_type_two == as.character(subset$plant_functional_type_two[j]))
          length_of_pft_1_uniques<-length(as.character(unique(subset_2$plant_functional_type_one)))
          length_of_pft_2_uniques<-length(as.character(unique(subset_2$plant_functional_type_two)))
          
          if(length_of_pft_2_uniques>1 | length_of_pft_1_uniques>1){
            aggregated_2<- TRUE
            }
          
          if(aggregated_1 && aggregated_2){is_legal_table<-FALSE }
        }
        
      }
      
      return(is_legal_table)
    } else{
      PEcAn.logger::logger.severe("Not every species or plant_functional_type is accounted for in custom_table provided. Please account for", missing, "and make sure that 'plant_fucntional_type_one' is matches to 'observation_one'")
    }
  
  }
}


 

 
