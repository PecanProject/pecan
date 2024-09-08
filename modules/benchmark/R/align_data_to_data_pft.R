#################################################################
#'align_data_to_data_pft
#'@details
#' Aligns vectors of Plant Fucntional Typed and species.
#' Can align: 
#' - two vectors of plant functional types (pft's) if a custom map is provided
#' - a list of species (usda, fia, or latin_name format) to a plant functional type
#' - a list of species in a custom format, with a table mapping it to bety_species_id's
#' 
#'  Will return a list of what was originally provided, bety_species_codes if possible, 
#'  and an aligned output. Because some alignement is order-sensitive, alignment based on observation_one
#'  and observation_two are both provided. 
#'
#'
#' @param con database connection
#' @param observation_one a vector of plant functional types, or species
#' @param observation_two another vector of plant functional types, or species
#' @param custom_table a table that either maps two pft's to one another or maps custom species codes to bety id codes.
#' In the second case, must be passable to match_species_id. 
#' @param format_one The output of query.format.vars() of observation one of the form output$vars$bety_names
#' @param format_two The output of query.format.vars() of observation two of the form output$vars$bety_names
#' @param subset_is_ok When aligning two species lists, this allows for alignment when species lists aren't identical.
#' set to FALSE by default. 
#' @return \code{list} containing the following columns:
#' \describe{
#'  \item{\code{$original}}{Will spit back out original vectors pre-alignment}
#'  \item{\code{$aligned$aligned_by_observation_one}}{Where possible, will return a vector of observation_one pft's/species in the order of observation_two}
#'  \item{\code{species}}{{Where possible, will return a vector of observation_two's pft's/species in the order of observation_one}}
#'  \item{\code{$bety_species_id}}{Where possible, will return the bety_species_id's for one or both observations}
#'  \item{\code{$bety_species_intersection}}{Where possible, will return the intersection of two aligned lists of species. subset_is_ok must be set to TRUE.}
#' }
#' @author Tempest McCabe
#' @examples \dontrun{
#' 
#' observation_one<-c("AMCA3","AMCA3","AMCA3","AMCA3")
#' observation_two<-c("a", "b", "a", "a")
#' 
#' table<-list()
#' table$plant_functional_type_one<- c("AMCA3","AMCA3","ARHY", "ARHY")
#' table$plant_functional_type_two<- c('a','a','b', 'b') # PFT groupings
#' table<-as.data.frame(table)
#'
#' format_one<-"species_USDA_symbol"
#' format_two<-"plant_functional_type"
#' 
#' aligned <- align_data_to_data_pft(
#'  con = con,
#'  observation_one = observation_one, observation_two = observation_two,
#'  format_one = format_one, format_two = format_two,
#'  custom_table = table)
#' }
#' @export

align_data_to_data_pft<-function(con, observation_one, observation_two, custom_table=NULL, format_one, format_two, subset_is_ok=FALSE){

  translation_table<-NULL
  bety_codes_one<-NA
  bety_codes_two<-NA
  bety_species_intersection<-NA
  
  if(check_if_species_list(format_one) && check_if_species_list(format_two)){  #Both are lists of species
    
    if (get_species_list_standard(format_one) == "custom" | get_species_list_standard(format_two) == "custom"){translation_table<-custom_table}
    
    bety_codes_one<-PEcAn.data.land::match_species_id(input_codes=observation_one, format_name= get_species_list_standard(format_one),translation_table = translation_table, bety=con)
    bety_codes_two<-PEcAn.data.land::match_species_id(input_codes=observation_two, format_name= get_species_list_standard(format_two), translation_table = translation_table,bety=con)
    
    if(setequal(bety_codes_one, bety_codes_two)){ #check if identical lists. 
      
      aligned_by_one<-bety_codes_two #Since they are identical, this has the same names as one, but in the order of two
      aligned_by_two<-bety_codes_one
      
    }else if(subset_is_ok){
      
      #for the case where intersections are ok, making columns where a species is present in on list but not the other NA's
  
      bety_species_intersection<-dplyr::intersect(bety_codes_one$bety_species_id,bety_codes_two$bety_species_id)
    
      bety_codes_one$bety_species_id[bety_codes_one$bety_species!=bety_species_intersection]<-NA
      bety_codes_two$bety_species_id[bety_codes_two$bety_species!=bety_species_intersection]<-NA
      
      aligned_by_one<-bety_codes_two$bety_species_id
      aligned_by_two<-bety_codes_one$bety_species_id
      
      
    }else{
      PEcAn.logger::logger.warn("These observations cannot be aligned, as they have different species lists. Returning NULL. Check species lists, or  set 'subset_is_ok' to TRUE. ")
      return(NULL)
    }
    
  }else if(check_if_species_list(format_one) && !check_if_species_list(format_two)){
    
    if(is.null(custom_table)){
      
      PEcAn.logger::logger.severe("Please provide custom_table")
      
    }else if (!is.null(custom_table)){
        
      if(check_if_legal_table(custom_table, observation_one, observation_two)){
        
        if (get_species_list_standard(format_one)=="custom"){translation_table<-custom_table}
        
        bety_codes_one<-PEcAn.data.land::match_species_id(input_codes=observation_one, format_name= get_species_list_standard(format_one),translation_table = translation_table, bety=con)
        
        aligned_by_one<-align_by_first_observation(observation_one,observation_two, custom_table)
        aligned_by_two<-align_by_first_observation(observation_two,observation_one, custom_table)
        
        
      }else{
        PEcAn.logger::logger.severe("custom_table provided does not correctly map plant_functional_type_one to plant_functional_type_two. One or more rows are mapped to multiple plant functional types.")
      } 
    }
    
  }else if(!check_if_species_list(format_one) && check_if_species_list(format_two)){
    
    if(is.null(custom_table)){PEcAn.logger::logger.severe("Please provide custom_table")}else if (!is.null(custom_table))
    {
      if(check_if_legal_table(custom_table, observation_one, observation_two)){
        
        if (get_species_list_standard(format_two)=="custom"){
          translation_table<-custom_table
        }
          
        bety_codes_two<-PEcAn.data.land::match_species_id(input_codes=observation_two, format_name= get_species_list_standard(format_two),translation_table = translation_table,bety=con)
        
        aligned_by_one<-align_by_first_observation(observation_one,observation_two, custom_table)
        aligned_by_two<-align_by_first_observation(observation_two,observation_one, custom_table)
        
      }else{
        PEcAn.logger::logger.severe("custom_table provided does not correctly map plant_functional_type_one to plant_functional_type_two. One or more rows are mapped to multiple plant functional types.")
      } 
    }
  
    return(aligned_species_list)
    
  }else if(check_if_list_of_pfts(format_one) && (check_if_list_of_pfts(format_two))){
  
    if(is.null(custom_table)){PEcAn.logger::logger.severe("Please provide custom_table")}else if (!is.null(custom_table))
    {
      if(check_if_legal_table(custom_table, observation_one, observation_two)){

        aligned_by_one<-align_by_first_observation(observation_one,observation_two, custom_table)
        aligned_by_two<-align_by_first_observation(observation_two,observation_one, custom_table)
        
      }else{
        PEcAn.logger::logger.severe("custom_table provided does not correctly map plant_functional_type_one to plant_functional_type_two. One or more rows are mapped to multiple plant functional types.")
      } 
    }
    
  }else{
    PEcAn.logger::logger.severe("PFTs are not in the correct format. Observations must have variables compatible with check_if_species_list(), or use the 'plant_functional_type' variable")
  }
  
  aligned_species_list<-list()
  aligned_species_list$bety_species_id$observation_one<-bety_codes_one
  aligned_species_list$bety_species_id$observation_two<-bety_codes_two
  aligned_species_list$original$observation_one<-observation_one
  aligned_species_list$original$observation_two<-observation_two
  aligned_species_list$aligned$aligned_by_observation_one<-aligned_by_one
  aligned_species_list$aligned$aligned_by_observation_two<-aligned_by_two
  
  return(aligned_species_list)
  
}
