##' Function queries a DB to extract veg info downstream
##' @name extract_veg
##' @title extract_veg
##' 
##' @param new_site new_site object passed from ic_process includes lat, lon, id, and name
##' @param start_date "YYYY-MM-DD"
##' @param end_date "YYYY-MM-DD"
##' @param source taken from input$source, passed from ic_process
##' @param gridres only used for source = "FIA"
##' @param format_name DEFAULT=NULL
##' @param machine_host passed from ic_process
##' @param dbparms taken from settings object, passed from ic_process
##' @param outfolder passed from ic_process, location where to store files
##' @param overwrite DEFAULT = FALSE
##' @param input_veg passed from input object in ic_process
##' @param ... Additional parameters
##'
##' @return results object to be passed back to get.veg.module
##' @export
##' @author Istem Fer and Alexis Helgeson
extract_veg <- function(new_site, start_date, end_date, 
                        source, gridres, format_name = NULL, 
                        machine_host, dbparms, outfolder, overwrite = FALSE, input_veg = input_veg, ...){
  #code taken from https://stackoverflow.com/questions/14183766/match-fun-provide-error-with-functions-defined-inside-functions
   # fget <- function(name, env = parent.frame()) {
   #    if (identical(env, emptyenv())) {
   #       stop("Could not find function called ", name, call. = FALSE)
   #    }
   #    
   #    if (exists(name, env, inherits = FALSE) && is.function(env[[name]])) {
   #       env[[name]]
   #    } else {
   #       fget(name, parent.env(env))
   #    }
   # }
 #--------------------------------------------------------------------------------------------------#
 # Extract veg info
   #set start and end date as date objects
   start_date = as.Date(start_date)
   end_date = as.Date(end_date)
   #keep this code chunk future PR will integrate back ask Alexis
 # fcnx <- paste0("extract_", source) # e.g. extract_FIA
 # #Need a better way to check if the function exists
 # if (!exists(fcnx)) {
 #   PEcAn.logger::logger.severe(paste(fcnx, "does not exist."))
 # }else{
 #   fcn <- fget(fcnx) #Error cannot find the function
 # }
 # extract_* functions need to have standard args
 lon <- as.numeric(new_site$lon)
 lat <- as.numeric(new_site$lat)
 #veg_info <- fcn(lon = lon, lat = lat, startdate = start_date, enddate = end_date, gridres, dbparms)
 
 if (source == "NEON_veg") {
   veg_info <- extract_NEON_veg(lon = lon, lat = lat, startdate = start_date, enddate = end_date, gridres, dbparms)
 } else if(source == "FIA"){
   veg_info <- extract_FIA(lon = lon, lat = lat, startdate = start_date, enddate = end_date, gridres, dbparms)
 }else{
   PEcAn.logger::logger.debug("Only have extract functions for source = NEON_veg or FIA, please use load_veg")

  #grabs named function and returns error if function cannot be found
  fget <- function(name, env = parent.frame()) {

      if (identical(env, emptyenv())) {
         stop("Could not find function called ", name, call. = FALSE)
      }
      
      if (exists(name, env, inherits = FALSE) && is.function(env[[name]])) {
         env[[name]]
      } else {
         fget(name, parent.env(env))
      }
   }
 #--------------------------------------------------------------------------------------------------#
 # Extract veg info
 fcnx <- paste0("extract_", source) # e.g. extract_FIA
 
 fcn_exist <- try(fcn <- do.call("::", list(paste0("PEcAn.data.land"), paste0(fcnx))))
 
 #detect if function exist
 if(is.character(fcn_exist)){
   PEcAn.logger::logger.severe(paste(fcnx, "does not exist."))
 }
 
 #--------------------------------------------------------------------------------------------------#
 # Match species
 if (source == "NEON_veg") {
    #skip species matching for now revisit later 
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
 }else{
    obs <- veg_info[[2]]
    
    # TODO: generalize this as we have more sources that have their own DB like FIA
    if(is.null(format_name)){
       if(source == "FIA"){
          format_name <- "fia" 
          code_col    <-  "spcd"
       }else{
          code_col    <- "species_USDA_symbol"
          format_name <- "usda"
          obs[obs$species_USDA_symbol != "2PLANT", ] #removes the rows with 2PLANT, this is a NEON specific code that means they could not identify the species 
       }
    }
    
    
    # match code to species ID
    spp.info <- match_species_id(input_codes = obs[[code_col]], format_name = format_name)
    
    # merge with data
    tmp <- spp.info[ , colnames(spp.info) != "input_code"]
    
    veg_info[[2]] <- cbind(obs, tmp)
    
    
    #--------------------------------------------------------------------------------------------------#
    # Write vegettion data as rds, return results to convert.input
    
    # need check for overwrite
    sppfilename <- PEcAn.data.land::write_veg(outfolder, start_date, veg_info = veg_info, source)
    
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

  }
  
  #--------------------------------------------------------------------------------------------------#
  # Match species
  
  obs <- veg_info[[2]]
  
  # TODO: generalize this as we have more sources that have their own DB like FIA
  if(is.null(format_name)){
    if(source == "FIA"){
      format_name <- "fia" 
      code_col    <-  "spcd"
    }else{
      code_col    <- "species_USDA_symbol"
      format_name <- "usda"
      obs <- obs[obs$species_USDA_symbol != "2PLANT" &  
                   obs$species_USDA_symbol != "2PLANT-H", ] #removes the rows with 2PLANT, this is a NEON specific code that means they could not identify the species 
   }

 }

  }
 # match code to species ID
 spp.info <- match_species_id(input_codes = obs[[code_col]], format_name = format_name)
 
 # merge with data
 tmp <- spp.info[ , colnames(spp.info) != "input_code"]
 
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

  
} # extract_veg
