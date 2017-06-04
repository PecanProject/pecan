##' Function uses load_data{benchmark} to get veg data
##' @name load_veg
##' @title load_veg
##' @export
##' @author Istem Fer
load_veg <- function(new_site, start_date, end_date, 
                        source_id, format_name = NULL, dbparms, outfolder, overwrite = FALSE, ...){
  
  
  bety <- dplyr::src_postgres(dbname   = dbparms$bety$dbname, 
                              host     = dbparms$bety$host, 
                              user     = dbparms$bety$user, 
                              password = dbparms$bety$password)

  #--------------------------------------------------------------------------------------------------#
  # Load data : this step requires DB connections 
  
  # query data.path from source id [input id in BETY]
  query      <- paste0("SELECT * FROM dbfiles where container_id = ", source_id)
  input_file <- db.query(query, con = bety$con)
  data_path  <- file.path(input_file[["file_path"]], input_file[["file_name"]])
  
  # query format info
  format <- query.format.vars(bety = bety, input.id = source_id)
  
  # load_data{benchmark}
  obs <- load_data(data.path = data_path, format, site = new_site)
  
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
      logger.severe("Can't match code to species. Please provide 'match.format' via settings.")
    }
  }else{
    logger.severe("Can't match code to species. No valid format found.")
  } 
  
  # match code to species ID
  # no lower case
  obs[[code.col]] <- toupper(obs[[code.col]])
  
  # filter dead trees
  obs <- remove_dead_trees(obs, code.col)
  spp.info <- match_species_id(input_codes = obs[[code.col]], format_name = format.name, bety = bety)
  
  # merge with data
  tmp <- spp.info[ , colnames(spp.info) != "input_code"]
  
  # a hack for now to have a similar structure as the FIA case
  veg_info      <- list() 
  veg_info[[1]] <- NULL   # the first sublist can be for the metadata maybe?
  veg_info[[2]] <- cbind(obs, tmp)
  
  
  #--------------------------------------------------------------------------------------------------#
  # Write vegettion data as rds, return results to convert.input
  
  # need check for overwrite
  sppfilename <- write_veg(outfolder, start_date, end_date, veg_info = veg_info, 
                           site_name = new_site$name, source)
  
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


.remove_dead_trees <- function(obs, code.col, by.code = TRUE){
  
  if(by.code){
    # remove by code
    dead_tree_codes <- c("2TB", "SNAG", "DEAD")
    fobs <- obs[!(obs[[code.col]] %in% dead_tree_codes), ]
  }else{
    # remove by mortality status?
  }
  
  return(fobs)
}