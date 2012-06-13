#--------------------------------------------------------------------------------------------------#
##' Query available trait data associated with a given pft and a list of traits
##'
##' @title Query trait data
##' @param spstr string of species id's from BETYdb database
##' @param priors vector of parameters for which priors have been specified 
##' @param con 
##' @return dataframe with trait data
##' @seealso \code{\link{query.bety.trait.data}}
##' @examples
##' spstr <- query.pft_species('ebifarm.c4crop')$spstr
##' trvec <- c('leafN', 'SLA')
##' trait.data <- query.bety.traits(spstr, trvec)
#--------------------------------------------------------------------------------------------------#
query.traits <- function(spstr, priors, con = NULL){

  if(is.null(con)){
    con <- query.base.con(...)
  }
  if(is.list(con)){
    print("query.traits")
    print("WEB QUERY OF DATABASE NOTE IMPLEMENTED")
    return(NULL)
  }
  
  query <- paste("select distinct variables.name from traits join variables 
                 on (traits.variable_id = variables.id) where specie_id in (", spstr,");", sep = "")
  query.result <- dbSendQuery(con, query)
  traits <- fetch(query.result, n = -1)$name
 
  traits <- unique(traits[traits %in% priors])
  
  ### Grab trait data
  trait.data <- lapply(traits, function(trait) query.trait.data(trait, spstr, con=con))
  names(trait.data) <- traits
  return(trait.data)
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.          		
####################################################################################################