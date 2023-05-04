############ Retrives soil data from gssurgo
#' This function queries the gSSURGO database for a series of map unit keys
#'
#' @param mukeys map unit key from gssurgo
#' @param fields a character vector of the fields to be extracted. See details and the default argument to find out how to define fields.
#'
#' @return a dataframe with soil properties. Units can be looked up from database documentation
#'
#' @details 
#' Full documention of available tables and their relationships can be found here \url{www.sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}
#' There have been occasions where NRCS made some minor changes to the structure of the API which this code is where those changes need
#' to be implemneted here.
#' Fields need to be defined with their associate tables. For example, sandtotal is a field in chorizon table which needs to be defined as chorizon.sandotal_(r/l/h), where 
#' r stands for the representative value, l stands for low and h stands for high. At the moment fields from mapunit, component, muaggatt, and chorizon tables can be extracted.
#'
#' @examples
#' \dontrun{
#'  PEcAn.data.land::gSSURGO.Query(
#'    mukeys = 2747727,
#'    fields = c(
#'      "chorizon.cec7_r", "chorizon.sandtotal_r",
#'      "chorizon.silttotal_r","chorizon.claytotal_r",
#'      "chorizon.om_r","chorizon.hzdept_r","chorizon.frag3to10_r",
#'      "chorizon.dbovendry_r","chorizon.ph1to1h2o_r",
#'      "chorizon.cokey","chorizon.chkey"))
#' }
#' @export
#'
gSSURGO.Query <- function(mukeys,
                          fields = c("chorizon.sandtotal_r",
                                     "chorizon.silttotal_r",
                                     "chorizon.claytotal_r")) {

  ######### Retrieve soil

  # Avoids duplicating fields that are always included in the query
  fixed_fields <- c("mapunit.mukey", "component.cokey", "component.comppct_r")
  qry_fields <- unique(fields[!(fields %in% fixed_fields)])
  
  body <- paste('<?xml version="1.0" encoding="utf-8"?>
               <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
               <soap:Body>
               <RunQuery xmlns="http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx">
               <Query>
               SELECT ',
                 paste(fixed_fields, collapse = ", "),
                 paste(qry_fields, collapse = ", "),
                 ' from mapunit
               join muaggatt on mapunit.mukey=muaggatt.mukey
               join component on mapunit.mukey=component.mukey
               join chorizon on component.cokey=chorizon.cokey
               where mapunit.mukey in (', paste(mukeys,collapse = ", "),');
               </Query>
               </RunQuery>
               </soap:Body>
               </soap:Envelope>')

  out <- httr::POST(
    url = "https://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx",
    config = list(
      httr::accept("text/xml"),
      httr::accept("multipart/*"),
      httr::add_headers(
        SOAPAction = "http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery")),
    httr::content_type("text/xml; charset=utf-8"), # I expected this to belong inside `config`, but doesn't seem to work there...
    encode="multipart",
    body = body)
  httr::stop_for_status(out)
  result <- httr::content(out, "text")

  suppressWarnings(
    suppressMessages({
      xml_doc <- XML::xmlTreeParse(result)
      xmltop  <- XML::xmlRoot(xml_doc)
      tablesxml <- (xmltop[[1]]["RunQueryResponse"][[1]]["RunQueryResult"][[1]]["diffgram"][[1]]["NewDataSet"][[1]])
    })
  )
  
  #parsing the table  
  tryCatch({
    suppressMessages(
      suppressWarnings({
        tables <- XML::getNodeSet(tablesxml,"//Table")
        
        ##### All datatables below newdataset
        dfs <- purrr::map_dfr(
            tables,
            function(tbl){
              lst <- purrr::map(
                XML::xmlToList(tbl),
                function(v)ifelse(is.null(v), NA, v)) #avoid dropping empty columns

              lst[names(lst) != ".attrs"]}
          )
          dfs <- dplyr::mutate(dfs, dplyr::across(dplyr::everything(), as.numeric))
      })
    )
    
    
    return(dfs)
  },
  error=function(cond) {
    print(cond)
    return(NULL)
  })
  
}


