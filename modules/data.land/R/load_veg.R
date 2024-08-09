#' load_veg
#'
#' uses `PEcAn.benchmark::load_data()` to get veg data
#'
#' @param new_site list passed to `load_data`
#' @param start_date,end_date date range to look up
#' @param source_id input id to look up in DB
#' @param source name of data source (used in file naming)
#' @param icmeta metadata for initial conditions
#' @param format_name file format to look for
#' @param machine_host hostname of machine where the data lives
#' @param dbparms parameters to use when opening connection to database
#' @param outfolder path to write results
#' @param overwrite Logical: replace existing files? NOTE: Currently ignored!
#' @param ... Additional arguments, currently ignored
#'
#' @export
#' @author Istem Fer
load_veg <- function(new_site, start_date, end_date,
                     source_id, source, icmeta = NULL, format_name = NULL,
                     machine_host, dbparms, outfolder, overwrite = FALSE, ...){

  con <- PEcAn.DB::db.open(dbparms$bety)
  on.exit(PEcAn.DB::db.close(con), add = TRUE)
  #--------------------------------------------------------------------------------------------------#
  # Load data : this step requires DB connections

  # get machine id
  machine_id <- PEcAn.DB::get.id(table = "machines", colnames = "hostname",
                       values = machine_host, con = con)

  # query data.path from source id [input id in BETY]
  query      <- paste0("SELECT * FROM dbfiles where container_id =  ", source_id,
                       "AND machine_id=", machine_id)

  input_file <- PEcAn.DB::db.query(query, con = con)
  data_path  <- file.path(input_file[["file_path"]], input_file[["file_name"]]) #File path and file name of source file from bety


  # query format info
  format     <- PEcAn.DB::query.format.vars(bety = con, input.id = source_id)

  # load_data{benchmark}
  obs        <- PEcAn.benchmark::load_data(data.path = data_path, format, site = new_site)


  #--------------------------------------------------------------------------------------------------#
  # Match species : this step requires DB connections

  if("species_USDA_symbol" %in% format$vars$bety_name){
    code.col    <- "species_USDA_symbol"
    format_name <- "usda"
  }else if("latin_name" %in% format$vars$bety_name){
    # not encountered an actual case yet, put here as a reminder
    code.col <- "latin_name"
    format_name <- "latin_name"
    # might indicate a custom format, should be passed to function
    if(is.null(format_name)){
      PEcAn.logger::logger.severe("Can't match code to species. Please provide 'match.format' via settings.")
    }
  }else{
    PEcAn.logger::logger.severe("Can't match code to species. No valid format found.")
  }
  # match code to species ID
  # no lower case
  obs[[code.col]] <- toupper(obs[[code.col]])
  spp.info <- match_species_id(input_codes = obs[[code.col]], format_name = format_name, bety = con)
  # merge with data
  tmp <- spp.info[ , colnames(spp.info) != "input_code"]

  # a hack for now to have a similar structure as the FIA case
  veg_info      <- list()

  if(!is.null(icmeta)){
    # the first sublist can be for the metadata maybe?
    # to be handled by veg2model later
    veg_info[[1]] <- icmeta
    if(is.null(icmeta$area)){
      # this might not be needed for all models but let's put a warning here before it's too late
      PEcAn.logger::logger.warn("IMPORTANT : No area info passed via metadata,
                                if your model needs plot area in IC calculations please provide it under 'settings$run$inputs$css$metadata$area'.")
    }
  }else{
    veg_info[[1]] <- NULL
    # this might not be needed for all models but let's put a warning here before it's too late
    PEcAn.logger::logger.warn("IMPORTANT : No area info passed via metadata,
                              if your model needs plot area in IC calculations please provide it under 'settings$run$inputs$css$metadata$area'.")
  }

  veg_info[[2]] <- cbind(obs, tmp)

  #--------------------------------------------------------------------------------------------------#
  # Write vegettion data as rds, return results to convert_input

  # need check for overwrite
  sppfilename <- write_veg(outfolder, start_date, veg_info = veg_info, source)
  # Build results dataframe for convert_input
  results <- data.frame(file = sppfilename,
                        host = machine_host,
                        mimetype = "application/rds",
                        formatname = "spp.info",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = basename(sppfilename),
                        stringsAsFactors = FALSE)
  ### return for convert_inputs
  return(invisible(results))


} # load_veg
