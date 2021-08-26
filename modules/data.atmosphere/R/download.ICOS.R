#' Download ICOS Ecosystem data products
#'
#' Currently available products: 
#' Drought-2018 ecosystem eddy covariance flux product https://www.icos-cp.eu/data-products/YVR0-4898
#' ICOS Final Fully Quality Controlled Observational Data (Level 2) https://www.icos-cp.eu/data-products/ecosystem-release
#'
#'
#' @param sitename ICOS id of the site. Example - "BE-Bra"
#' @param outfolder path to the directory where the output file is stored. If specified directory does not exists, it is created.
#' @param start_date start date of the data request in the form YYYY-MM-DD
#' @param end_date end date area of the data request in the form YYYY-MM-DD
#' @param product ICOS product to be downloaded. Currently supported options: "Drought2018", "ETC"
#' @param overwrite should existing files be overwritten. Default False.
#' @param ... used when extra arguments are present.
#' @return information about the output file
#' @export
#' @examples
#' \dontrun{
#' download.ICOS("FI-Sii", "/home/carya/pecan", "2016-01-01", "2018-01-01", product="Drought2018")
#' }
#' @author Ayush Prasad
#'
download.ICOS <-
  function(sitename,
           outfolder,
           start_date,
           end_date,
           product,
           overwrite = FALSE, ...) {

    # make sure output folder exists
    if (!file.exists(outfolder)) {
      dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
    }


    download_file_flag <- TRUE
    extract_file_flag <- TRUE
    sitename <- sub(".* \\((.*)\\)", "\\1", sitename)
    
    if (tolower(product) == "drought2018") {
      # construct output CSV file name
      output_file_name <-
        paste0(
          "FLX_",
          sitename,
          "_FLUXNET2015_FULLSET_HH_"
        )
      
      # construct zip file name
      zip_file_name <-
        paste0(outfolder, "/Drought", sitename, ".zip")
      
      # data type, can be found from the machine readable page of the product
      data_type <-
        "http://meta.icos-cp.eu/resources/cpmeta/dought2018ArchiveProduct"
      
      file_name <-
        paste0('FLX_', sitename, '_FLUXNET2015_FULLSET_HH')
      
      format_name <- "ICOS_ECOSYSTEM_HH"
      
    } else if (tolower(product) == "etc") {
      output_file_name <-
        paste0("ICOSETC_", sitename, "_FLUXNET_HH_01.csv")
      
      # construct zip file name
      zip_file_name <-
        paste0(outfolder, "/ICOSETC_Archive_", sitename, ".zip")
      
      # data type, can be found from the machine readable page of the product
      data_type <-
        "http://meta.icos-cp.eu/resources/cpmeta/etcArchiveProduct"
      
      file_name <-
        paste0("ICOSETC_", sitename, "_FLUXNET_HH")
      
      format_name <- "ICOS_ECOSYSTEM_HH"
      
    } else {
      PEcAn.logger::logger.severe("Invalid product. Product should be one of 'Drought2018', 'ETC' ")
    }
    
    output_file <- list.files(path = outfolder, pattern = output_file_name)
    if(length(output_file != 0) && !overwrite){
        PEcAn.logger::logger.info("Output CSV file for the requested site already exists")
        download_file_flag <- FALSE
        extract_file_flag <- FALSE
        output_file_name <- output_file
    }

    if (extract_file_flag &&
        file.exists(zip_file_name) && !overwrite) {
      PEcAn.logger::logger.info("Zip file for the requested site already exists, extracting it...")
      download_file_flag <- FALSE
      extract_file_flag <- TRUE
    }
    
    if (download_file_flag) {
      # Find dataset product id by using the site name
      
      # ICOS SPARQL end point
      url <- "https://meta.icos-cp.eu/sparql?type=JSON"
      
      # RDF query to find out the information about the data set using the site name
      body <- "
      prefix cpmeta: <http://meta.icos-cp.eu/ontologies/cpmeta/>
      prefix prov: <http://www.w3.org/ns/prov#>
      select ?dobj ?spec ?timeStart ?timeEnd
      where {
      	VALUES ?spec {<data_type>}
      	?dobj cpmeta:hasObjectSpec ?spec .
      	VALUES ?station {<http://meta.icos-cp.eu/resources/stations/ES_sitename>}
      			?dobj cpmeta:wasAcquiredBy/prov:wasAssociatedWith ?station .
      ?dobj cpmeta:hasStartTime | (cpmeta:wasAcquiredBy / prov:startedAtTime) ?timeStart .
      ?dobj cpmeta:hasEndTime | (cpmeta:wasAcquiredBy / prov:endedAtTime) ?timeEnd .
      	FILTER NOT EXISTS {[] cpmeta:isNextVersionOf ?dobj}
      }
      "
      body <- gsub("data_type", data_type, body)
      body <- gsub("sitename", sitename, body)
      response <- httr::POST(url, body = body)
      response <- httr::content(response, as = "text")
      response <- jsonlite::fromJSON(response)
      dataset_url <- response$results$bindings$dobj$value
      dataset_start_date <-
        lubridate::as_datetime(
          strptime(response$results$bindings$timeStart$value, format = "%Y-%m-%dT%H:%M:%S")
        )
      dataset_end_date <-
        lubridate::as_datetime(
          strptime(response$results$bindings$timeEnd$value, format = "%Y-%m-%dT%H:%M:%S")
        )
      if (is.null(dataset_url)) {
        PEcAn.logger::logger.severe("Data is not available for the requested site")
      }
      if (dataset_start_date > lubridate::as_datetime(start_date)) {
        PEcAn.logger::logger.severe(
          paste(
            "Data is not available for the requested start date. Please try again with",
            dataset_start_date,
            "as start date."
          )
        )
      }
      
      if (dataset_end_date < lubridate::as_date(end_date)) {
        PEcAn.logger::logger.severe(
          paste(
            "Data is not available for the requested end date. Please try again with",
            dataset_end_date,
            "as end date."
          )
        )
      }
      dataset_id <- sub(".*/", "", dataset_url)
      
      # construct the download URL
      download_url <-
        paste0('https://data.icos-cp.eu/licence_accept?ids=%5B%22',
               dataset_id,
               '%22%5D')
      # Download the zip file
      file <-
        httr::GET(url = download_url,
                  httr::write_disk(zip_file_name,
                                   overwrite = TRUE),
                  httr::progress())
    }
    
    if (extract_file_flag) {
      # extract only the hourly data file
      zipped_csv_name <-
        grep(
          paste0('*', file_name),
          utils::unzip(zip_file_name, list = TRUE)$Name,
          ignore.case = TRUE,
          value = TRUE
        )
      utils::unzip(zip_file_name,
                   files = zipped_csv_name,
                   junkpaths = TRUE,
                   exdir = outfolder)
      if (tolower(product) == "drought2018") {
        output_file_name <- zipped_csv_name
      }else if (tolower(product) == "etc") {
        # reformat file slightly so that both Drought2018 and ETC files can use the same format
        tmp_csv <- utils::read.csv(file.path(outfolder, output_file_name))
        new_tmp <- cbind(tmp_csv[, -which(colnames(tmp_csv)=="LW_OUT")], tmp_csv[, which(colnames(tmp_csv)=="LW_OUT")])
        colnames(new_tmp) <- c(colnames(tmp_csv)[-which(colnames(tmp_csv)=="LW_OUT")], "LW_OUT")
        utils::write.csv(new_tmp, file = file.path(outfolder, output_file_name), row.names = FALSE)
      }
    }
    
    
    # get start and end year of data from file
    firstline <-
      system(paste0("head -2 ", file.path(outfolder, output_file_name)), intern = TRUE)
    firstline <- firstline[2]
    lastline <-
      system(paste0("tail -1 ", file.path(outfolder, output_file_name)), intern = TRUE)
    
    firstdate_st <- paste0(
      substr(firstline, 1, 4),
      "-",
      substr(firstline, 5, 6),
      "-",
      substr(firstline, 7, 8),
      " ",
      substr(firstline, 9, 10),
      ":",
      substr(firstline, 11, 12)
    )
    lastdate_st <- paste0(
      substr(lastline, 1, 4),
      "-",
      substr(lastline, 5, 6),
      "-",
      substr(lastline, 7, 8),
      " ",
      substr(lastline, 9, 10),
      ":",
      substr(lastline, 11, 12)
    )
    
    
    rows    <- 1
    results <- data.frame(
      file = character(rows),
      host = character(rows),
      mimetype = character(rows),
      formatname = character(rows),
      startdate = character(rows),
      enddate = character(rows),
      dbfile.name = substr(basename(output_file_name), 1, nchar(basename(output_file_name)) - 4),
      stringsAsFactors = FALSE
    )
    
    results$file[rows]       <-
      file.path(outfolder, output_file_name)
    results$host[rows]       <- PEcAn.remote::fqdn()
    results$startdate[rows]  <- firstdate_st
    results$enddate[rows]    <- lastdate_st
    results$mimetype[rows]   <- "text/csv"
    results$formatname[rows] <- format_name
    
    return(results)
    
  }
