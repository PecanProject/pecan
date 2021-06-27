#' Download ICOS Drought 2018 data
#'
#' Link to the product: https://www.icos-cp.eu/data-products/YVR0-4898
#'
#' Variables present in the output CSV file: TA_F, SW_IN_F, LW_IN_F, VPD_F, PA_F, P_F, WS_F, WD, RH, PPFD_IN, CO2_F_MDS, TS_F_MDS_1, TS_F_MDS_2, NEE_VUT_REF, LE_F_MDS, RECO_NT_VUT_REF and GPP_NT_VUT_REF
#'
#' @param sitename ICOS id of the site. Example - "BE-Bra"
#' @param outfolder path to the directory where the output file is stored. If specified directory does not exists, it is created.
#' @param start_date start date of the data request in the form YYYY-MM-DD
#' @param end_date end date area of the data request in the form YYYY-MM-DD
#' @param overwrite should existing files be overwritten. Default False.
#' @return information about the output file
#' @export
#' @examples 
#' \dontrun{
#' download.Drought2018("FI-Sii", "/home/carya/pecan", "2016-01-01", "2018-01-01")
#' }
#' @author Ayush Prasad
#' 
download.Drought2018 <-
  function(sitename,
           outfolder,
           start_date,
           end_date,
           overwrite = FALSE) {
    
    # construct zip file name
    zip_file_name <- paste0(outfolder, "/Drought", sitename, ".zip")
    
    stage_download <- TRUE
    
    if (file.exists(zip_file_name) && !overwrite) {
      PEcAn.logger::logger.info("Zip file for the requested site already exists, extracting it...")
      stage_download <- FALSE
    }
    
    if (as.Date(end_date) > as.Date("2018-12-31")) {
      PEcAn.logger::logger.severe(paste0(
        "Requested end date ", end_date, " exceeds Drought 2018 availability period"
      ))
    }
    
    if (stage_download) {
      # Find dataset product id by using the site name
      
      # ICOS SPARQL end point
      url <- "https://meta.icos-cp.eu/sparql?type=JSON"
      
      # RDF query to find out the information about the data set using the site name
      body <- "
      prefix cpmeta: <http://meta.icos-cp.eu/ontologies/cpmeta/>
      prefix prov: <http://www.w3.org/ns/prov#>
      select ?dobj ?spec ?timeStart 
      where {
      	VALUES ?spec {<http://meta.icos-cp.eu/resources/cpmeta/dought2018ArchiveProduct>}
      	?dobj cpmeta:hasObjectSpec ?spec .
      	VALUES ?station {<http://meta.icos-cp.eu/resources/stations/ES_sitename>}
      			?dobj cpmeta:wasAcquiredBy/prov:wasAssociatedWith ?station .
      ?dobj cpmeta:hasStartTime | (cpmeta:wasAcquiredBy / prov:startedAtTime) ?timeStart .
      	FILTER NOT EXISTS {[] cpmeta:isNextVersionOf ?dobj}
      }
      "
      body <- gsub("sitename", sitename, body)
      response <- httr::POST(url, body = body)
      response <- httr::content(response, as = "text")
      response <- jsonlite::fromJSON(response)
      dataset_url <- response$results$bindings$dobj$value
      dataset_start_date <-  as.Date(strptime(response$results$bindings$timeStart$value, format = "%Y-%m-%dT%H:%M:%S"))
      if(is.null(dataset_url)){
        PEcAn.logger::logger.severe("Data is not available for the requested site")
      }
      if(dataset_start_date > as.Date(start_date)){
        PEcAn.logger::logger.severe(paste("Data is not available for the requested start date. Please try again with", dataset_start_date, "as start date.") )
      }
      dataset_id <- sub(".*/", "", dataset_url)
      
      # construct the download URL
      download_url <-
        paste0('https://data.icos-cp.eu/licence_accept?ids=%5B%22',
               dataset_id,
               '%22%5D')
      # Download the zip file
      file <-
        httr::GET(url = download_url, httr::write_disk(
          paste0(outfolder, "/Drought", sitename, ".zip"),
          overwrite = TRUE
        ), httr::progress())
    }
    
    file_name <- paste0('FLX_', sitename, '_FLUXNET2015_FULLSET_HH')
    
    # extract only the hourly data file
    zipped_csv_name <-
      grep(
        paste0('^', file_name),
        utils::unzip(zip_file_name, list = TRUE)$Name,
        ignore.case = TRUE,
        value = TRUE
      )
    utils::unzip(file.path(outfolder, paste0('Drought', sitename, '.zip')),
          files = zipped_csv_name,
          exdir = outfolder)
    `%>%` <- dplyr::`%>%`
    # read in the output CSV file and select the variables required
    df <- utils::read.csv(file.path(outfolder, zipped_csv_name))
    df <-
      subset(
        df,
        select = c(
          "TIMESTAMP_START",
          "TA_F",
          "SW_IN_F",
          "LW_IN_F",
          "VPD_F",
          "PA_F",
          "P_F",
          "WS_F",
          "WD",
          "RH",
          "PPFD_IN",
          "CO2_F_MDS",
          "TS_F_MDS_1",
          "TS_F_MDS_2",
          "NEE_VUT_REF",
          "LE_F_MDS",
          "RECO_NT_VUT_REF",
          "GPP_NT_VUT_REF"
        )
      )
    df <-
      df %>% dplyr::filter((
        as.Date(strptime(df$TIMESTAMP_START, format = "%Y%m%d%H%M")) >= as.Date(start_date) &
          as.Date(strptime(df$TIMESTAMP_START, format = "%Y%m%d%H%M")) <= as.Date(end_date)
      ))
    # save the csv file
    utils::write.csv(df, file.path(outfolder, zipped_csv_name), row.names = FALSE)
    
    rows    <- 1
    results <- data.frame(
      file = character(rows),
      host = character(rows),
      mimetype = character(rows),
      formatname = character(rows),
      startdate = character(rows),
      enddate = character(rows),
      dbfile.name = zipped_csv_name,
      stringsAsFactors = FALSE
    )
    
    results$file[rows]       <- file.path(outfolder, zipped_csv_name)
    results$host[rows]       <- PEcAn.remote::fqdn()
    results$startdate[rows]  <- start_date
    results$enddate[rows]    <- end_date
    results$mimetype[rows]   <- "text/csv"
    results$formatname[rows] <- "Drought2018_HH"
    
    return(results)
    
  }