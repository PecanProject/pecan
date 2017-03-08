##' @name extract_FIA
##' @title extract_FIA
##' @export
write_veg <- function(outfolder, start_date, end_date, temp_file, ...){
  
  #--------------------------------------------------------------------------------------------------#
  # Load and delete file
  load(temp_file)
  file.remove(temp_file)
  
  # convert to matrix !JUST FOR NOW, THIS IS WRONG!
  veg_info <- sapply(seq_along(veg_info), function(x) apply(veg_info[[x]], 2, as.numeric))
  
  #--------------------------------------------------------------------------------------------------#
  # Write ncdf files and return results data frame
  
  start_year    <- lubridate::year(start_date)
  end_year      <- lubridate::year(end_date)
  out_file      <- paste("FIA", start_year, end_year, "veg", "nc", sep = ".")
  out_file_full <- file.path(outfolder, out_file)
  
  # declare netcdf dimensions and variables 
  # dummy dimensions, just testing
  # will come up with a better structure for this file
  pt <- ncdf4::ncdim_def("pt", units = "", vals = 1:nrow(veg_info[[1]]))
  pc <- ncdf4::ncdim_def("pc", units = "", vals = 1:ncol(veg_info[[1]]))
  ct <- ncdf4::ncdim_def("ct", units = "", vals = 1:nrow(veg_info[[2]]))
  cc <- ncdf4::ncdim_def("cc", units = "", vals = 1:ncol(veg_info[[2]])) 
  
  var <- list()
  var[[1]] <- ncdf4::ncvar_def("pss_info", units = "", dim = list(pt, pc), missval = -999)
  var[[2]] <- ncdf4::ncvar_def("css_info", units = "", dim = list(ct, cc), missval = -999)
  
  # write
  nc <- ncdf4::nc_create(out_file_full, var)
  varfile <- file(out_file_full, "w")
  for (i in seq_along(var)) {
    ncdf4::ncvar_put(nc, var[[i]], veg_info[[i]])
    cat(paste(var[[i]]$name), file = varfile, sep = "\n")
  }
  close(varfile)
  ncdf4::nc_close(nc)
  
  # Build results dataframe for convert.input
  results <- data.frame(file = out_file_full, 
                        host = c(fqdn()), 
                        mimetype = "application/x-netcdf", 
                        formatname = "spp.info", 
                        startdate = start_date, 
                        enddate = end_date, 
                        dbfile.name = out_file, 
                        stringsAsFactors = FALSE)
  
  return(invisible(results))
  
} # write_veg