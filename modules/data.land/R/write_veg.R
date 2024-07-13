##' Function to save intermediate rds file
##' 
##' @name write_veg
##' @title write_veg
##' @param outfolder output folder
##' @param start_date start date
##' @param veg_info vegetation data to be saved
##' @param source name of data source (used in file naming)
##' @export
write_veg <- function(outfolder, start_date, veg_info, source){
  
  start_year    <- lubridate::year(start_date)
  out_file      <- paste(source, start_year, "veg", "rds", sep = ".")
  out_file_full <- file.path(outfolder, out_file)
  
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  
  saveRDS(veg_info, file = out_file_full)
  
  return(out_file_full)
  
} # write_veg

# Maybe a remove_dead_trees function
