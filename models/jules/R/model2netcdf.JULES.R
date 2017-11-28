#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert MODEL output into the PEcAn standard
##' 
##' @name model2netcdf.JULES
##' @title Code to convert JULES output into netCDF format
##' @param outdir Location of model output
##' @export
##' @author Michael Dietze
model2netcdf.JULES <- function(outdir) {
  files <- dir(outdir, pattern = ".nc$", full.names = TRUE)
  dumps <- files[grep(pattern = "dump", files)]
  files <- setdiff(files, dumps)
  
  print(files)
  for (fname in files) {
    print(fname)
    nc <- ncdf4::nc_open(fname, write = TRUE)
    ## extract variable and long names
    write.table(sapply(nc$var, function(x) { x$longname }), 
                file = paste0(fname, ".var"), 
                col.names = FALSE, 
                row.names = TRUE,
                quote = FALSE)
    ## JULES time is in seconds; convert to DOY
    time <- ncdf4::ncvar_get(nc, "time") / 86400
    ncdf4::ncvar_put(nc, "time", time)
    ncdf4::nc_close(nc)
    dir.create(file.path(outdir,"dump"))
    for(dump in dumps){
      file.rename(dump, file.path(dirname(dump),"dump",basename(dump)))
    }
  }
} # model2netcdf.JULES
