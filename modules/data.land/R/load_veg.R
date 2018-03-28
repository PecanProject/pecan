##' Function uses load_data{benchmark} to get veg data
##' @name load_veg
##' @title load_veg
##' @export
##' @author Istem Fer
load_veg <- function(new_site, start_date, end_date, 
                     source_id, source, icmeta = NULL, format_name = NULL, 
                     machine_host, dbparms, outfolder, overwrite = FALSE, ...){
  
  
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)

  #--------------------------------------------------------------------------------------------------#
  # Load data : this step requires DB connections 
  
  # get machine id
  machine_id <- get.id(table = "machines", colnames = "hostname", 
                       values = machine_host, con = bety$con)
  
  # query data.path from source id [input id in BETY]
  query      <- paste0("SELECT * FROM dbfiles where container_id =  ", source_id,
                       "AND machine_id=", machine_id)
  
  input_file <- PEcAn.DB::db.query(query, con = bety$con)
  data_path  <- file.path(input_file[["file_path"]], input_file[["file_name"]])
  
  # query format info
  format     <- PEcAn.DB::query.format.vars(bety = bety, input.id = source_id)
  
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
  
  spp.info <- match_species_id(input_codes = obs[[code.col]], format_name = format_name, bety = bety)
  
  # merge with data
  tmp <- spp.info[ , colnames(spp.info) != "input_code"]
  
  # a hack for now to have a similar structure as the FIA case
  veg_info      <- list() 
  
  if(!is.null(icmeta)){
    # the first sublist can be for the metadata maybe?
    # to be handled by veg2model later
    veg_info[[1]] <- icmeta
  }else{
    veg_info[[1]] <- NULL 
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


