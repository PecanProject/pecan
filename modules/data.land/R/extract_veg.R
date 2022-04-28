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
 #Need a better way to check if the function exists
 if (!exists(fcnx)) {
   PEcAn.logger::logger.severe(paste(fcnx, "does not exist."))
 }else{
   fcn <- fget(fcnx)
 }
 # extract_* functions need to have standard args
 if(source == "NEON_veg"){
    #extract_NEON_veg needs a location to store downloaded NEON files, this is not a standard argument, so this if/else statement is a hack but it is meant to ensure the extract_veg function works
    store_dir <- input_veg$storedir
    lon <- as.numeric(new_site$lon)
    lat <- as.numeric(new_site$lat)
    veg_info <- extract_NEON_veg(lon = new_site$lon, lat = new_site$lat, start_date, end_date, store_dir = store_dir)
 }else{
    lon <- as.numeric(new_site$lon)
    lat <- as.numeric(new_site$lat)
    veg_info <- fcn(lon = lon, lat = lat, start_date, end_date, gridres, dbparms) 
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
 
  
} # extract_veg
