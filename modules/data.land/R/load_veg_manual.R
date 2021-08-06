##' @description  Function uses load_data{benchmark} to get veg data
##'
##' @param new_site data frame, id/lat/lon/name info about the site
##' @param start_date date in "YYYY-MM-DD" format, in case of source==FIA it's the settings$run$start.date, otherwise start_date of the IC file in DB
##' @param end_date date in "YYYY-MM-DD" format, in case of source==FIA it's the settings$run$end.date, otherwise end_date of the IC file in DB
##' @param source_id taken from input_veg. Numeric ID of the input in the PEcAn database  
##' @param source taken from input_veg. The input data type. This tag name needs to match the names in the corresponding conversion functions. If you are using PEcAn’s automatic input processing, this is the only field you need to set. However, this field is ignored if id and/or path are provided.
##' @param icmeta default NULL. Info taken from settings$run$inputs$css$metadata$area.
##' @param format_name default NULL. Info taken from format$vars$bety_name
##' @param machine_host local machine hostname, e.g. "pecan2.bu.edu"
##' @param dbparms list, settings$database info reqired for opening a connection to DB
##' @param outfolder path to where the processed files will be written
##' @param overwrite Default is FALSE. logical flag for convert.input
##' @param ... Additional parameters
##'
##' @name load_veg_manual
##' @title load_veg_manual
##' @export
##' @author Istem Fer
load_veg_manual <- function(new_site, start_date, end_date, 
                     source_id, source, icmeta = NULL, format_name = NULL, 
                     machine_host, dbparms, outfolder, overwrite = FALSE, ...){
  library(neonstore)
  library(neonUtilities)
  
  start_date = as.Date("2020-01-01")
  end_date = as.Date("2021-09-01")
  outfolder = "/projectnb/dietzelab/ahelgeso/NEON_ic_data/"
  source = "NEON"
  machine_host = "localhost"
#Load in NEON datasets
#Only need to run neon_download once
  #neonstore::neon_download("DP1.10098.001", table = NA, site = c("HARV"),start_date = start_date, end_date = end_date, type = "basic",api = "https://data.neonscience.org/api/v0")
  temp.veg <- neonUtilities::stackFromStore(filepaths=neon_dir(),dpID="DP1.10098.001",pubdate="2021-06-01",package="basic")
  joined.veg <- dplyr::left_join(temp.veg$vst_mappingandtagging, temp.veg$vst_apparentindividual, by = "individualID")

#Filter joined.veg for required information: DBH, tree height, and species
  filter.veg <- dplyr::select(joined.veg, siteID.x, plotID.x, subplotID.x, taxonID, scientificName, taxonRank, date.y, stemDiameter, height)
#Filter for most recent record
  filter.date <- dplyr::filter(filter.veg, date.y >= start_date)
#Create year column
  filter.date$year <- format(as.Date(filter.date$date.y, format="%d/%m/%Y"),"%Y")
#Rename NEON column names to match pecan functions
  colnames(filter.date) <- c("site_name", "plot", "Subplot", "species_USDA_symbol", "species", "taxonRank", "date", "DBH", "height", "year")
#Set filter.date as obs
  obs <- filter.date
  
  icmeta <- list(area = 400)
#Create connection with BETY
  dbparms <- list()
    dbparms$bety$dbname <- "bety"
    dbparms$bety$host <- "psql-pecan.bu.edu"
    dbparms$bety$user <- "bety"
    dbparms$bety$password <- "bety"
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)
  
  con <- bety$con  
  #match species names in filter.date to species ID on BETY
  
  #Add BETY species IDs to filter.date
  
  #Add plot size to filter.date
  
  #--------------------------------------------------------------------------------------------------#
  # Match species : this step requires DB connections 
  
  # if("species_USDA_symbol" %in% obs){
  #   code.col    <- "species_USDA_symbol"
  #   format_name <- "usda"
  # }else if("latin_name" %in% obs){
  #   # not encountered an actual case yet, put here as a reminder
  #   code.col <- "latin_name"
  #   format_name <- "latin_name" 
  #   # might indicate a custom format, should be passed to function
  #   if(is.null(format_name)){
  #     PEcAn.logger::logger.severe("Can't match code to species. Please provide 'match.format' via settings.")
  #   }
  # }else{
  #   PEcAn.logger::logger.severe("Can't match code to species. No valid format found.")
  # } 
  # match code to species ID
  # no lower case
  code.col    <- "species_USDA_symbol"
  format_name <- "usda"
  obs[[code.col]] <- toupper(obs[[code.col]])
  spp.info <- PEcAn.data.land::match_species_id(input_codes = obs[[code.col]], format_name = format_name, bety = bety)
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
  # Write vegettion data as rds, return results to convert.input
  
  # need check for overwrite
  sppfilename <- write_veg(outfolder, start_date, veg_info = veg_info, source)
  # Build results dataframe for convert.input
  results <- data.frame(file = sppfilename, 
                        host = machine_host, 
                        mimetype = "application/rds", 
                        formatname = "spp.info", 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = basename(sppfilename), 
                        stringsAsFactors = FALSE)
  ### return for convert.inputs
  return(invisible(results))  
  
  
} # load_veg


