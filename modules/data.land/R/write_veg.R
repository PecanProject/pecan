##' @name extract_FIA
##' @title extract_FIA
##' @export
write_veg <- function(outfolder, start_date, end_date, veg_info, overwrite = FALSE, ...){
  
  #--------------------------------------------------------------------------------------------------#
  # Save rds file and return results data frame
  
  start_year    <- lubridate::year(start_date)
  end_year      <- lubridate::year(end_date)
  out_file      <- paste("FIA", start_year, end_year, "veg", "rds", sep = ".")
  out_file_full <- file.path(outfolder, out_file)
  
  saveRDS(veg_info, file = out_file_full)
  
  # Build results dataframe for convert.input
  results <- data.frame(file = out_file_full, 
                        host = c(fqdn()), 
                        mimetype = "application/rds", 
                        formatname = "spp.info", 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = out_file, 
                        stringsAsFactors = FALSE)
  
  return(invisible(results))
  
} # write_veg