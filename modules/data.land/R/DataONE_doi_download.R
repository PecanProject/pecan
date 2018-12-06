#' format_identifier
#'
#' @param id the doi or other identifier linked to the package in DataONE
#'
#' @return returns the id in the proper format for querying the DataONE Federation (using solrQuery syntax)
#' @export 
#'
#' @author Liam P Burke, \email{lpburke@@bu.edu}
#' @description This function is for formatting purposes. It simply inserts the doi or id that the user wishes to query into Solr format so that it is compatible with the dataoneR query functionality in the PEcAn function 
#' 
#' @examples 
format_identifier = function(id){ 
  doi.template <- 'id:"_"' # solr format
  doi1 <<- base::gsub("_", id, doi.template) # replace "_" with the doi or id and store in global environment
  return(doi1) 
} # end function

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

#' id_resolveable 
#'
#' @param id the doi or other identifier linked to the package in DataONE  
#' @param CNode CNode="PROD"
#' @param return_result boolean that returns or suppresses result of query. defaults to TRUE. 
#' @description Uses dataone::query from dataoneR to query DataONE. Prints result if data exists
#' 
#' @return returns message indicating wether or not the id resolves to data in the DataONE federation and information about said data. 
#' @export
#'
#' @examples
id_resolveable = function(id, return_result = TRUE, CNode = "PROD"){
  format_identifier(id) # reformat the id in solr format
  
  cn <- dataone::CNode(CNode) 
  queryParams <- list(q=doi1, rows="5") 
  result <- dataone::query(cn, solrQuery = queryParams, as = "data.frame") # return query results as a data.frame
  
  if(return_result == TRUE){ # option that displays data.frame of query
    print(result)
  }
  
  if(is.null(result[1,1])){ # if there is no data available, result[1,1] will return a NULL value
    return("doi does not resolve in the DataOne federation and therefore cannot be retrieved by doi.
             Either download this data locally and import using PEcAn's drag and drop feature, or search DataOne manually for another data identifier. Thank you for your patience.")
  } else{
    return("data can be found in D1 federation")
  }
} # end function

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

#' get_resource_map
#'
#' @param id the doi or other identifier linked to the package in DataONE  
#' @param CNode default is "PROD"
#' @description Locates data in DataONE and returns the resource_map or a message indicating that there is no corresponding resource_map for the given id
#' 
#' @return return the resource_map or a message indicating that there is no corresponding resource_map for the given id
#' @export
#'
#' @examples
get_resource_map = function(id, CNode = "PROD"){
  cn <- dataone::CNode(CNode) 
  locations <- dataone::resolve(cn, pid = id) 
  mnId <<- locations$data[1,"nodeIdentifier"] # store mnId in global environment
  mn <<- dataone::getMNode(cn, mnId) # store mn in global environment
  
  format_identifier(id) # format the identifier in solr Query format
  queryParamList <- list(q=doi1, fl="resourceMap") # custom query for the resourceMap
  resource_map_df <- dataone::query(cn, solrQuery = queryParamList, as="data.frame") 
  resource_map <<- resource_map_df[1,1] # store resource map in global env. resource map is always in resource_map_df[1,1]
  
  if (is.null(resource_map_df[1,1])){ # inform user if id/ doi has a corresponding resource_map or if this needs to be found manually
    print("doi does not resolve a resource_map. Please manually search for the resource_map in DataONE search: https://search.DataONE.org/#data")
  } else{
    print("Continue to next phase to complete download")
    return(resource_map)
  }
} # end function

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

#' download_packages
#'
#' @param resource_map the resource map that corresponds to the given data package
#' @param CNode defaults to "PROD"
#' @param download_format typically "application/bagit-097". Other possible formats currently unknown.
#' @param overwrite_directory boolean that indicates whether or not the function should overwrite the directory
#' @param directory location that download.packages places the data
#'
#' @description Uses resource_map and dataone::getPackage to download the data into a BagItFile. Then utils::unzip unzips the data and stores in the user's directory. 
#' @return results of download
#' @export
#'
#' @examples
download_package_rm = function(resource_map, directory, CNode = "PROD", download_format = "application/bagit-097", 
                               overwrite_directory = TRUE){
  # Finding the mnId (query)
  cn <- dataone::CNode(CNode) 
  locations <- dataone::resolve(cn, pid = resource_map) 
  mnId <<- locations$data[1,"nodeIdentifier"]
  
  # download the bagitFile 
  mn <<- dataone::getMNode(cn, mnId)
  bagitFile <<- dataone::getPackage(mn, id = resource_map, format = download_format)
  
  zip_contents <<- utils::unzip(bagitFile, files = NULL, list = FALSE, overwrite = overwrite_directory, # Unzip the bagitFile and store in directory specified under exdir
        junkpaths = FALSE, exdir = directory, unzip = "internal",
        setTimes = FALSE)
  return(zip_contents)
} # end function


