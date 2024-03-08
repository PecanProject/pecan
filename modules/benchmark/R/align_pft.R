#' Align vectors of Plant Functional Type and species.
#'
#'@details
#' Can align: 
#' - two vectors of plant fucntional types (pft's) if a custom map is provided
#' - a list of species (usda, fia, or latin_name format) to a plant fucntional type
#' - a list of species in a custom format, with a table mapping it to bety_species_id's
#' 
#'  Will return a list of what was originally provided, bety_speceis_codes if possible, 
#'  and an aligned output. Becuase some alignement is order-sensitive, alignment based on observation_one
#'  and observation_two are both provided. 
#'
#'\code{comparison_type} can be one of the following:
#' \describe{
#'  \item{\code{data_to_data}}{Will align lists of pfts and species. Must be assosiated with inputs.}
#'  \item{\code{data_to_model}}{Not yet implemented}
#'  \item{\code{model_to_model}}{Not yet implemented}
#'  }
#'
#'
#' @param con database connection
#' @param observation_one a vector of plant fucntional types, or species
#' @param observation_two anouther vector of plant fucntional types, or species
#' @param custom_table a table that either maps two pft's to one anouther or maps custom species codes to bety id codes. 
#' In the second case, must be passable to match_species_id. 
#' @param format_one The output of query.format.vars() of observation one of the form output$vars$bety_names
#' @param format_two The output of query.format.vars() of observation two of the form output$vars$bety_names
#' @param subset_is_ok When aligning two species lists, this allows for alignement when species lists aren't identical. 
#' set to FALSE by default. 
#' @param comparison_type one of "data_to_model", "data_to_data", or "model_to_model"
#' @param ... other arguments, currently ignored
#'
#' @return \code{list} containing the following columns:
#' \describe{
#'  \item{\code{$original}}{Will spit back out original vectors pre-alignment}
#'  \item{\code{$aligned$aligned_by_observation_one}}{Where possible, will return a vector of observation_one pft's/species in the order of observation_two}
#'  \item{\code{species}}{{Where possible, will return a vector of observation_two's pft's/species in the order of observation_one}}
#'  \item{\code{$bety_species_id}}{Where possible, will return the bety_species_id's for one or both observations}
#' }
#'
#' @author Tempest McCabe
#' @examples \dontrun{
#' 
#' 
#' #------------ A species to PFT alignment -----------
#' observation_one<-c("AMCA3","AMCA3","AMCA3","AMCA3")
#' observation_two<-c("a", "b", "a", "a") #
#' 
#' format_one<-"species_USDA_symbol"
#' format_two<-"plant_funtional_type"
#' 
#' table<-list()
#' table$plant_functional_type_one<- c("AMCA3","AMCA3","ARHY", "ARHY")
#' table$plant_functional_type_two<- c('a','a','b', 'b') # PFT groupings
#' table<-as.data.frame(table)
#'
#' 
#' aligned<-align_pft(con = con, observation_one = observation_one, observation_two = observation_two, 
#' format_one = format_one, format_two = format_two, custom_table = table)
#' }
#' 
#' @export
align_pft<-function(con, observation_one, observation_two, custom_table=NULL, format_one, format_two, subset_is_ok=FALSE, comparison_type="data_to_data", ...){

  if(comparison_type == "data_to_model"){
    
    #align_data_to_model_pft(settings_one, observations_1)
    PEcAn.logger::logger.severe("data_to_model alignment not yet implemented. Returning NULL.")
  
    
  }else if (comparison_type == "data_to_data"){
    
    align_data_to_data_pft(con, observation_one, observation_two, custom_table, format_one, format_two, subset_is_ok=FALSE)
    
  }else if (comparison_type == "model_to_model"){
    
    #align_model_to_model_pft(settings_one, settings_two)
    PEcAn.logger::logger.severe("model_to_model alignment not yet implemented. Returning NULL.")
    
    
  }else{
    PEcAn.logger::logger.severe("comparison_type must be set to either 'data_to_model', 'data_to_data', or model_to_model")
  }

}
