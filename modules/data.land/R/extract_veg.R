##' Function queries a DB to extract veg info downstream
##' @name extract_veg
##' @title load_veg
##' @export
##' @author Istem Fer
extract_veg <- function(new_site, start_date, end_date, 
                     source, gridres, format_name = NULL, 
                     machine_host, dbparms, outfolder, overwrite = FALSE, ...){
  
  
 #--------------------------------------------------------------------------------------------------#
 # Extract veg info
  
 fcnx <- paste0("extract_", source) # e.g. extract_FIA
 
 if (!exists(fcnx)) {
   PEcAn.logger::logger.severe(paste(fcnx, "does not exist."))
 }else{
   fcn <- match.fun(fcnx)
 }
 
 # extract_* functions need to have standard args
 veg_info <- fcn(lon = new_site$lon, lat = new_site$lat, start_date, end_date, gridres, dbparms)
 
 
 #--------------------------------------------------------------------------------------------------#
 # Match species
 
 obs <- veg_info[[2]]
 
 # TODO: generalize this as we have more sources that have their own DB like FIA
 if(is.null(format_name)){
   format_name <- "fia" 
   code_col    <-  "spcd"
 }

 
 # match code to species ID
 spp.info <- match_species_id(input_codes = obs[[code_col]], format_name = format_name)
 
 # merge with data
 tmp <- spp.info[ , colnames(spp.info) != "input_code"]
 
 veg_info[[2]] <- cbind(obs, tmp)
 
 
 #--------------------------------------------------------------------------------------------------#
 # Write vegettion data as rds, return results to convert.input
 
 # need check for overwrite
 sppfilename <- write_veg(outfolder, start_date, end_date, veg_info = veg_info, source)
 
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
