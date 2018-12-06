##' @name pool_ic_list2netcdf
##' @title pool_ic_list2netcdf
##' @description Converts input list containing standard dimensions and variables (named values) for initial conditions to a netcdf file, input to pool-based models.
##' @export
##'
##' @param input list with two elements: list of netcdf dimensions (dims, with named values) and list of variables (vals, with named values)
##' @param outdir directory to write netcdf file
##' @param siteid site id
##' @author Anne Thomas

pool_ic_list2netcdf <- function(input, outdir, siteid){
  if(is.null(input$vals) || length(input$vals) == 0){
    PEcAn.logger::logger.severe("Please provide 'vals' list in input with variable names assigned to values")
  }
  
  if(is.null(input$dims) || length(input$dims) == 0){
    if (any(sapply(input$vals,length) > 1)){
      PEcAn.logger::logger.severe("A variable has length > 1; please provide non-empty 'dims' list in input")
    }
  }
  #to do: check
  
  dims <- list()
  for(dimname in names(input$dims)){
    vals <- input$dims[[which(names(input$dims) == dimname)]]
    ncdim = PEcAn.utils::to_ncdim(dimname, vals)
    dims[[dimname]] <- ncdim
  }
  
  ncvars <- lapply(names(input$vals), PEcAn.utils::to_ncvar, dims)
  
  #create nc file
  str_ns <- paste0(siteid %/% 1e+09, "-", siteid %% 1e+09)
  basefile <- paste0("IC_site_", str_ns)
  outfile <- file.path(outdir, paste0(basefile,".nc"))
  nc  <- ncdf4::nc_create(outfile, ncvars)
  
  #put variables in nc file
  for (i in seq(ncvars)) {
    varname <- ncvars[[i]]$name
    ncdf4::ncvar_put(nc, ncvars[[i]], input$vals[[varname]])
  }
  
  #close file
  ncdf4::nc_close(nc)
  
  #create results object
  results <- data.frame(file = outfile,
                        host = PEcAn.remote::fqdn(), 
                        mimetype = "application/x-netcdf", 
                        formatname = "pool_initial_conditions",
                        startdate = NA, 
                        enddate = NA, 
                        dbfile.name = basefile, 
                        stringsAsFactors = FALSE)
  
  return(results)
} #pool_ic_list2netcdf
