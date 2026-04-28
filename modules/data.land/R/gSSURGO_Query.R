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
#' | `chorizon.dbthirdbar_r`| Bulk density at field capacity            | g/cm3        |
#' | `chorizon.ph1to1h2o_r` | Soil pH (1:1 H2O)                         | pH (unitless)|
#' | `chorizon.cokey`       | Component key (identifier)                | -            |
#' | `chorizon.chkey`       | Horizon key (identifier)                  | -            |
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

#' Maximum area for SSURGO API requests
SSURGO_API_MAX_AREA_M2 <- 10100000000  # nolint: object_name_linter

#' Get map unit keys (mukeys) from gSSURGO
#'
#' These functions query the NRCS gSSURGO Web Feature Service to retrieve map
#' unit keys based on different spatial filters.
#'
#' @param bbox Numeric vector of length 4: c(xmin, ymin, xmax, ymax) in WGS84
#'   (EPSG:4326). Features that intersect the bounding box are returned.
#' @param point Numeric vector of length 2: c(lon, lat) in WGS84 (EPSG:4326).
#' @param distance Numeric. Distance in meters from the point. Use 0 for exact
#'   point intersection.
#'
#' @return Character vector of unique map unit keys (mukeys).
#'
#' @details
#' These functions use the NRCS SDM Data Access Web Feature Service:
#' \url{https://sdmdataaccess.nrcs.usda.gov/SpatialFilterHelp.htm}
#'
#' The total extent cannot exceed 10,100,000,000 square meters (~3,900 square
#' miles). Use `ssurgo_mukeys_bigbbox()` for large bounding boxes.
#'
#' @examples
#' \dontrun{
#' # Bounding box query
#' mukeys <- ssurgo_mukeys_bbox(bbox = c(-114.006, 32.1823, -113.806, 32.2823))
#'
#' # Point with distance (600m radius)
#' mukeys <- ssurgo_mukeys_point(point = c(-91.22, 38.46), distance = 600)
#'
#' # Large bounding box
#' mukeys <- ssurgo_mukeys_bigbbox(bbox = c(-120, 35, -110, 45))
#' }
#' @name ssurgo_mukeys
NULL

#' @rdname ssurgo_mukeys
#' @export
ssurgo_mukeys_bbox <- function(bbox) {
  if (!is.numeric(bbox) || length(bbox) != 4) {
    stop("bbox must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax)")
  }

  xmin <- bbox[1]
  ymin <- bbox[2]
  xmax <- bbox[3]
  ymax <- bbox[4]

  if (xmin >= xmax || ymin >= ymax) {
    stop("bbox must have xmin < xmax and ymin < ymax")
  }

  wgs84_crs <- sf::st_crs(4326)

  # Calculate the area of the bbox to make sure that it's smaller than the
  # SSURGO limit (`SSURGO_API_MAX_AREA_M2`).
  bbox_matrix <- rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )
  bbox_poly <- sf::st_polygon(list(bbox_matrix))
  bbox_sf <- sf::st_sfc(bbox_poly, crs = wgs84_crs)
  area <- as.numeric(sf::st_area(bbox_sf))

  if (area > SSURGO_API_MAX_AREA_M2) {
    stop(
      paste0(
        "Bounding box area (", format(area, scientific = FALSE),
        " m2) exceeds maximum allowed area (", format(SSURGO_API_MAX_AREA_M2, scientific = FALSE),
        " m2). Use ssurgo_mukeys_bigbbox() for large bounding boxes."
      )
    )
  }

  base_url <- "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMWGS84Geographic.wfs"

  query <- list(
    SERVICE = "WFS",
    VERSION = "1.1.0",
    REQUEST = "GetFeature",
    TYPENAME = "MapunitPoly",
    BBOX = paste(bbox, collapse = ","),
    OUTPUTFORMAT = "XMLMukeyList"
  )

  resp <- httr2::request(base_url) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_perform()

  httr2::resp_check_status(resp)

  mukeys <- unique(parse_mukey_response(resp))

  mukeys
}

#' @rdname ssurgo_mukeys
#' @export
ssurgo_mukeys_point <- function(point, distance) {
  if (length(point) != 2) {
    stop("point must be a numeric vector of length 2: c(lon, lat)")
  }

  if (!is.numeric(distance) || distance < 0) {
    stop("distance must be a non-negative numeric value")
  }

  lon <- point[1]
  lat <- point[2]

  circle_area <- pi * (distance^2)
  if (circle_area > SSURGO_API_MAX_AREA_M2) {
    stop(
      paste0(
        "Search radius area (", format(circle_area, scientific = FALSE),
        " m2) exceeds maximum allowed area (", format(SSURGO_API_MAX_AREA_M2, scientific = FALSE),
        " m2)."
      )
    )
  }

  filter_xml <- paste0(
    "<Filter>",
    "<DWithin>",
    "<PropertyName>Geometry</PropertyName>",
    "<gml:Point>",
    "<gml:coordinates>", lon, ",", lat, "</gml:coordinates>",
    "</gml:Point>",
    "<Distance units=\"m\">", distance, "</Distance>",
    "</DWithin>",
    "</Filter>"
  )

  base_url <- "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMWGS84Geographic.wfs"

  query <- list(
    SERVICE = "WFS",
    VERSION = "1.1.0",
    REQUEST = "GetFeature",
    TYPENAME = "MapunitPoly",
    FILTER = filter_xml,
    OUTPUTFORMAT = "XMLMukeyList"
  )

  resp <- httr2::request(base_url) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_perform()

  httr2::resp_check_status(resp)

  mukeys <- unique(parse_mukey_response(resp))

  mukeys
}

#' @rdname ssurgo_mukeys
#' @export
ssurgo_mukeys_bigbbox <- function(bbox) {
  if (!is.numeric(bbox) || length(bbox) != 4) {
    stop("bbox must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax)")
  }

  xmin <- bbox[1]
  ymin <- bbox[2]
  xmax <- bbox[3]
  ymax <- bbox[4]

  if (xmin >= xmax || ymin >= ymax) {
    stop("bbox must have xmin < xmax and ymin < ymax")
  }

  wgs84_crs <- sf::st_crs(4326)

  # Get the total bbox area.
  bbox_matrix <- rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )
  bbox_poly <- sf::st_polygon(list(bbox_matrix))
  bbox_sf <- sf::st_sfc(bbox_poly, crs = wgs84_crs)

  bbox_area <- as.numeric(sf::st_area(bbox_sf))

  width_deg <- xmax - xmin
  height_deg <- ymax - ymin

  aspect_ratio <- width_deg / height_deg

  n_cells <- ceiling(bbox_area / SSURGO_API_MAX_AREA_M2)
  cells_per_side <- sqrt(n_cells)

  ncol_cells <- ceiling(cells_per_side * sqrt(aspect_ratio))
  nrow_cells <- ceiling(cells_per_side / sqrt(aspect_ratio))

  if (ncol_cells < 1) ncol_cells <- 1
  if (nrow_cells < 1) nrow_cells <- 1

  grid_wgs84 <- sf::st_make_grid(
    bbox_sf,
    n = c(ncol_cells, nrow_cells),
    crs = wgs84_crs
  )

  cell_bboxes <- purrr::map(grid_wgs84, sf::st_bbox)

  base_url <- "https://sdmdataaccess.nrcs.usda.gov/Spatial/SDMWGS84Geographic.wfs"

  reqs <- purrr::map(cell_bboxes, function(cell_bbox) {
    cell_vec <- c(
      cell_bbox["xmin"], cell_bbox["ymin"],
      cell_bbox["xmax"], cell_bbox["ymax"]
    )
    query <- list(
      SERVICE = "WFS",
      VERSION = "1.1.0",
      REQUEST = "GetFeature",
      TYPENAME = "MapunitPoly",
      BBOX = paste(cell_vec, collapse = ","),
      OUTPUTFORMAT = "XMLMukeyList"
    )
    httr2::request(base_url) |>
      httr2::req_url_query(!!!query)
  })

  reqs_throttled <- reqs |>
    # max 10 tries per minute
    purrr::map(httr2::req_throttle, capacity = 10) |>
    # keep trying for 2 minutes before giving up
    purrr::map(httr2::req_retry, max_seconds = 120)

  resps <- httr2::req_perform_parallel(
    reqs_throttled,
    on_error = "continue",
    max_active = 10,
    progress = TRUE
  )

  parse_mukeys <- function(resp) {
    if (inherits(resp, "httr2_response")) {
      tryCatch({
        parse_mukey_response(resp)
      }, error = function(e) {
        character(0)
      })
    } else {
      character(0)
    }
  }

  mukeys_list <- purrr::map(resps, parse_mukeys)

  unique(unlist(mukeys_list, use.names = FALSE))
}

#' Parse responses from the mukey WFS service
#'
#' @param resp `httr2` response object from SSURGO mukey WFS API
#' @return character vector of mukeys
parse_mukey_response <- function(resp) {
  resp_text <- httr2::resp_body_string(resp)
  resp_xml <- XML::xmlParse(resp_text)
  mukey_nodes <- XML::getNodeSet(resp_xml, "//MapUnitKeyList")
  if (length(mukey_nodes) == 0) {
    return(character(0))
  }
  mukey_str <- XML::xmlValue(mukey_nodes[[1]])
  if (is.null(mukey_str) || nchar(trimws(mukey_str)) == 0) {
    return(character(0))
  }
  strsplit(trimws(mukey_str), ",")[[1]]
}
