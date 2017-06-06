##' @name write_veg
##' @title write_veg
##' @export
write_veg <- function(outfolder, start_date, end_date, veg_info, site_name, source){
  
  #--------------------------------------------------------------------------------------------------#
  # Save rds file and return results data frame
  
  start_year    <- lubridate::year(start_date)
  end_year      <- lubridate::year(end_date)
  if(start_year == end_year){
    out_file      <- paste(site_name, source, start_year, "veg", "rds", sep = ".")
  }else{
    out_file      <- paste(site_name, source, start_year, end_year, "veg", "rds", sep = ".")
  }
  
  out_file_full <- file.path(outfolder, out_file)
  
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  
  saveRDS(veg_info, file = out_file_full)
  
  return(out_file_full)
  
} # write_veg