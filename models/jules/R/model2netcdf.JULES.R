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
  statics <- files[grep(pattern = "STATIC", files)]
  files <- setdiff(setdiff(files, dumps),statics)
  
  print(files)
  for (fname in files) {
    print(fname)
    nc <- ncdf4::nc_open(fname, write = TRUE)
    ## extract variable and long names
    utils::write.table(sapply(nc$var, function(x) { x$longname }), 
                file = paste0(fname, ".var"), 
                col.names = FALSE, 
                row.names = TRUE,
                quote = FALSE)
    
    vars <- names(nc[["var"]])
    # Check that frac is reported
    if("frac_grid" %in% vars){
      frac <- ncdf4::ncvar_get(nc, "frac_grid")
    } else {
      PEcAn.logger::logger.warn("Surface type fraction is not an output and thus other outputs may not be parseable")
    }
    
    base_dims <- vapply(nc$var[["GPP"]]$dim, `[[`, character(1), "name")# This isn't the best example, except that GPP is the default with read.output (and it is not reported by surface type)
    for(i in seq_along(vars)){
      var <- vars[i]
      nonstd_var <- nrow(PEcAn.utils::standard_vars[which(PEcAn.utils::standard_vars$Variable.Name == var),]) == 0  
      dims <- vapply(nc$var[["GPP"]]$dim, `[[`, character(1), "name")
      diff_dims <- setdiff(dims,base_dims)
      if(length(diff_dims) > 0){# ie more than just x, y, time
        PEcAn.logger::logger.warn("Variable", vars[i], "has additional dimension", diff_dims, "attempting to aggregate and/or select appropriate data")
        
        if(diff_dims %in% c("pft","type")){
          # Value reported for multiple surface types (or PFTs)
          # Sum over all types, weighted by frac
          
          x_raw <- ncdf4::ncvar_get(nc, var)
          x <- matrix(0,nrow(x_raw),ncol(x_raw))
          for(j in 1:nrow(x_raw)){
            x[j,] <- x_raw[j,] * frac[j,]  
          }
          x <- colSums(x)
          
        }else if(diff_dims == "soil"){
          # Value reported for multiple soil layers
          # Select a default layer? Or integrate?
          
          x <- ncdf4::ncvar_get(nc, vars[i])[1,] # THIS IS A PLACEHOLDER
          
        }else{PEcAn.logger::logger.error("Can't properly convert", vars[i])}
        
        # If non-standard variable, we need to save the variable info for later
        if(nonstd_var) var_dump <- nc$var[[var]]
        
        # Have to delete the variable from the nc file
        # and add it over again because the dimensions have changed
        cmd <- sprintf("ncks -O -x -v %s %s %s", var, fname, fname)
        system(cmd)
        ncdf4::nc_close(nc)
        nc <- ncdf4::nc_open(fname, write = TRUE)
        # Check did the variable get deleted 
        if (!(var %in% names(nc[["var"]]))) {
          PEcAn.logger::logger.debug(var, "successfully removed from", fname)
        }
        dim = list(time = nc$dim$time, x = nc$dim$x, y = nc$dim$y)

        if(nonstd_var){
          nc_var <- ncdf4::ncvar_def(var, units = var_dump$units, dim = list(time = nc$dim$time), 
                                     missval = var_dump$missval,
                                     longname = var_dump$longname)
        }else{
          nc_var <- PEcAn.utils::to_ncvar(var, dim)
        }
        
        ncdf4::ncvar_add(nc, nc_var)
        ncdf4::nc_close(nc)
        nc <- ncdf4::nc_open(fname, write = TRUE) # Why do I have to close and reopen it?
        ncdf4::ncvar_put(nc, nc_var, x)
        
      } 
    }
    ## JULES time is in seconds; convert to DOY
    time <- ncdf4::ncvar_get(nc, "time") / 86400
    ncdf4::ncvar_put(nc, "time", time)
    ncdf4::nc_close(nc)
    dir.create(file.path(outdir,"dump"))
    for(dump in dumps){
      file.rename(dump, file.path(dirname(dump),"dump",basename(dump)))
    }
    dir.create(file.path(outdir,"STATIC"))
    for(static in statics){
      file.rename(static, file.path(dirname(static),"dump",basename(static)))
    }
  }
} # model2netcdf.JULES
