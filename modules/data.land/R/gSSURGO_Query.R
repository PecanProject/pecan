############ Retrives soil data from gssurgo
#' This function queries the gSSURGO database for a series of map unit keys
#'
#' @param mukeys map unit key from gssurgo
#' @param fields a character vector of the fields to be extracted. See details and the default argument to find out how to define fields.
#'
#' @return a dataframe with soil properties.
#'
#' @md
#' @details 
#' This function queries the NRCS gSSURGO database using map unit keys (mukeys).  
#'
#' * **Available tables**: `mapunit`, `component`, `muaggatt`, `chorizon`, and `chfrags`.  
#' * **Field definitions**: Fields must be specified with their associated table name.  
#'   For example, total sand content is stored in the `chorizon` table and must be
#'   requested as `chorizon.sandtotal_(r|l|h)`, where:
#'   - `r` = representative value  
#'   - `l` = low value  
#'   - `h` = high value  
#'
#' **Commonly queried fields and units** (see NRCS gSSURGO ["Tables and Columns Report"](https://www.nrcs.usda.gov/sites/default/files/2022-08/SSURGO-Metadata-Tables-and-Columns-Report.pdf) 
#' for full list):
#'
#' | Field                  | Description                               | Units        |
#' |------------------------|-------------------------------------------|--------------|
#' | `chorizon.cec7_r`      | Cation exchange capacity at pH 7          | cmol(+)/kg   |
#' | `chorizon.sandtotal_r` | Total sand (<2 mm fraction)               | %            |
#' | `chorizon.silttotal_r` | Total silt (<2 mm fraction)               | %            |
#' | `chorizon.claytotal_r` | Total clay (<0.002 mm fraction)           | %            |
#' | `chorizon.om_r`        | Organic matter (<2 mm soil)               | %            |
#' | `chorizon.hzdept_r`    | Horizon top depth                         | cm           |
#' | `chfrags.fragvol_r`    | Rock fragments                            | % (by volume)|
#' | `chorizon.dbthirdbar_r`| Bulk density at field capacity            | g/cm³        |
#' | `chorizon.ph1to1h2o_r` | Soil pH (1:1 H2O)                         | pH (unitless)|
#' | `chorizon.cokey`       | Component key (identifier)                | —            |
#' | `chorizon.chkey`       | Horizon key (identifier)                  | —            |
#'
#' **API stability:** The NRCS occasionally modifies the API schema. If queries fail,
#'   adjustments may be required here to align with the updated structure. 
#'
#' Full documentation of available tables and their relationships is provided in the
#' \href{https://sdmdataaccess.nrcs.usda.gov/QueryHelp.aspx}{gSSURGO documentation}.
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
#' @author Hamze Dokohaki, Akash
#' @export
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
                 paste(c(fixed_fields, qry_fields), collapse = ", "),
                 ' from mapunit
               join muaggatt on mapunit.mukey=muaggatt.mukey
               join component on mapunit.mukey=component.mukey
               join chorizon on component.cokey=chorizon.cokey
               left join chfrags on chorizon.chkey=chfrags.chkey
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


