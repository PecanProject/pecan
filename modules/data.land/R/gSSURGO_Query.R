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

  if (!requireNamespace("httr", quietly = TRUE)) {
    PEcAn.logger::logger.severe(
      "Package 'httr' is required for gSSURGO queries but is not installed.",
      "Please install it with: install.packages('httr')")
  }
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


#' Fetch gSSURGO soil data for an area of interest
#'
#' Retrieves soil property data from the USDA gSSURGO database using the soilDB package.
#' This function performs the data retrieval step separately from ensemble generation,
#' enabling inspection of raw database values and simpler unit testing.
#'
#' @param lat Latitude of center point (optional if aoi provided)
#' @param lon Longitude of center point (optional if aoi provided)
#' @param aoi Custom area of interest as sf or terra polygon (optional)
#' @param radius Buffer radius in meters around lat/lon point (default: 500)
#' @param depths Soil depth breakpoints in meters, must start with 0 
#'
#' @return A list containing:
#'   \item{soilprop}{Data frame with component-level soil properties}
#'   \item{mukey_counts}{Table of mapunit key pixel counts (for area weighting)}
#'   \item{depths_cm}{Depth breakpoints in centimeters}
#'
#' @export
#' @author Akash
#' @examples
#' \dontrun{
#'   result <- gssurgo_fetch_area(lat = 40.1, lon = -88.2)
#'   head(result$soilprop)
#' }
gssurgo_fetch_area <- function(lat = NULL, lon = NULL, aoi = NULL, 
                                radius = 500, depths = c(0, 0.15, 0.30, 0.60)) {
  
  # Validate inputs
  if (is.null(aoi) && (is.null(lat) || is.null(lon))) {
    PEcAn.logger::logger.severe("Must provide either 'aoi' OR both 'lat' and 'lon'")
  }
  
  # Create AOI from point + radius if needed
  if (is.null(aoi)) {
    aoi <- data.frame(lon = lon, lat = lat) %>%
      terra::vect(crs = "epsg:4326") %>%
      terra::buffer(width = radius)
  }
  
  # Validate depths parameter
  if (depths[1] != 0) {
    PEcAn.logger::logger.severe(
      "First depth must be 0. Use depths = c(0, 0.15, 0.30, ...) like hist() breaks."
    )
  }
  
  PEcAn.logger::logger.info("Querying gSSURGO Web Coverage Service for map unit keys")
  mu_raster <- soilDB::mukey.wcs(aoi = aoi, db = 'gSSURGO', res = 30)
  
  mukey_values <- terra::values(mu_raster)
  mukey_values <- mukey_values[!is.na(mukey_values)]
  mukey_counts <- table(mukey_values)
  mukeys_all <- as.character(names(mukey_counts))
  
  if (length(mukeys_all) == 0) {
    PEcAn.logger::logger.severe("No mapunit keys were found for this site.")
  }
  
  # Fetch complete soil data via soilDB
  sda_data <- tryCatch({
    soilDB::fetchSDA(
      WHERE = paste0("mukey IN (", paste(mukeys_all, collapse = ","), ")"),
      duplicates = TRUE,
      childs = TRUE,
      nullFragsAreZero = TRUE,
      rmHzErrors = TRUE
    )
  }, error = function(e) {
    PEcAn.logger::logger.error(paste("Failed to fetch SDA data:", e$message))
    return(NULL)
  })
  
  if (is.null(sda_data)) {
    PEcAn.logger::logger.error("Could not retrieve soil data from SDA")
    return(NULL)
  }
  
  hz_data <- aqp::horizons(sda_data)
  site_data <- aqp::site(sda_data)
  
  # Component-level aggregation by depth
  depths_cm <- depths * 100
  all_soil_data <- list()
  
  for (i in seq_len(length(depths_cm) - 1)) {
    top_depth <- depths_cm[i]
    bottom_depth <- depths_cm[i + 1]
    
    depth_hz <- hz_data %>%
      dplyr::filter(hzdept_r < bottom_depth & hzdepb_r > top_depth)
    
    if (nrow(depth_hz) == 0) next
    
    component_data <- depth_hz %>%
      dplyr::left_join(site_data[, c("cokey", "comppct_r", "mukey")], by = "cokey") %>%
      dplyr::mutate(
        hz_top_adj = pmax(hzdept_r, top_depth),
        hz_bot_adj = pmin(hzdepb_r, bottom_depth),
        hz_thickness = hz_bot_adj - hz_top_adj
      ) %>%
      dplyr::group_by(mukey, cokey, comppct_r) %>%
      dplyr::summarise(
        sandtotal_r = stats::weighted.mean(sandtotal_r, hz_thickness, na.rm = TRUE),
        silttotal_r = stats::weighted.mean(silttotal_r, hz_thickness, na.rm = TRUE),
        claytotal_r = stats::weighted.mean(claytotal_r, hz_thickness, na.rm = TRUE),
        om_r = stats::weighted.mean(om_r, hz_thickness, na.rm = TRUE),
        dbthirdbar_r = stats::weighted.mean(dbthirdbar_r, hz_thickness, na.rm = TRUE),
        fragvol_r = stats::weighted.mean(fragvol_r, hz_thickness, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        tex_sum = sandtotal_r + silttotal_r + claytotal_r,
        sandtotal_r = sandtotal_r / tex_sum * 100,
        silttotal_r = silttotal_r / tex_sum * 100,
        claytotal_r = claytotal_r / tex_sum * 100,
        hzdept_r = top_depth,
        hzdepb_r = bottom_depth
      ) %>%
      dplyr::select(-tex_sum)
    
    all_soil_data[[i]] <- component_data
  }
  
  soilprop <- do.call(rbind, all_soil_data)
  
  return(list(
    soilprop = soilprop,
    mukey_counts = mukey_counts,
    depths_cm = depths_cm
  ))
}
