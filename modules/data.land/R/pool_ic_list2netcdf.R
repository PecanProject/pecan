##' @name pool_ic_list2netcdf
##' @title pool_ic_list2netcdf
##' @export
##'
##' @param input list with two elements: list of netcdf dimensions (dims, with named values) and list of variables (vals, with named values)
##' @param outdir directory to write netcdf file
##' @param siteid site id
##' @author Anne Thomas

pool_ic_list2netcdf <- function(input, outdir,siteid){
  if(is.null(input$dims)){
    PEcAn.utils::logger.severe("Please provide 'dims' list in input, containing 'lon', 'lat', 'depth'")
  }
  if(is.null(input$vals)){
    PEcAn.utils::logger.severe("Please provide 'vals' list in input with variable names assigned to values")
  }
  if (!all(c("lon","lat","depth") %in% names(input$dims))){
    PEcAn.utils::logger.severe(paste("Missing or invalid dimname, please check if dims in input are named lon, lat, depth"))
  }
  
  ##define dimensions available for netcdf
  #assuming additional dim names aren't necessary; could use process similar to ncvars if so
  lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = input$dims$lon, 
                          longname = "station_longitude")
  lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = input$dims$lat, 
                          longname = "station_latitude")
  depth <- ncdf4::ncdim_def(name = "depth", units = "cm", vals = input$dims$depth, 
                            longname = "depth to bottom of layer")
  
  dims <- list(lon = lon, lat = lat, depth = depth) #depth soon to be replaced in standard table with depth
  
  #lookup table
  #standard_vars <- data(standard_vars,package = "PEcAn.utils") 
  standard_vars <- read.csv(system.file("data/standard_vars.csv",package="PEcAn.utils"),stringsAsFactors = FALSE)
  ###function to lapply to all variables in input list and return a list of ncvars
  to_ncvar <- function(varname,standard_vars,dims){
    #print(varname)
    var <- standard_vars[which(standard_vars$Variable.Name == varname),]
    #check var exists
    if(nrow(var)==0){
     PEcAn.utils::logger.severe(paste("Variable",varname,"not in standard_vars"))
      #return(NULL)
    }
    dimset <- var[,c("dim1","dim2","dim3","dim4")]
    dim <- dims[which(names(dims) %in% dimset)] #subset list of all dims for this variable
    #check that dim isn't 0
    if(length(dim)==0 || is.null(dim)){
      PEcAn.utils::logger.severe(paste("No dimensions were loaded for",varname))
    }
    units = as.character(var$Units) #if the units are a factor the function fails
    ncvar <- ncdf4::ncvar_def(name = varname, units = units, dim = dim, -999, prec = "double") #also add longname?
    if (!is.na(var$Long.name)) {
      ncvar$longname <- as.character(var$Long.name)
    }
    return(ncvar)
  }
  
  ncvars = lapply(names(input$vals),to_ncvar,standard_vars,dims)
  
  #create nc file
  str_ns <- paste0(siteid %/% 1e+09, "-", siteid %% 1e+09)
  outfile <- file.path(outdir, paste0("IC_site_", str_ns,".nc"))
  nc  <- ncdf4::nc_create(outfile, ncvars)
  
  #put variables in nc file
  for (i in seq(ncvars)) {
    varname <- ncvars[[i]]$name
    ncdf4::ncvar_put(nc, ncvars[[i]], input$vals[[varname]])
  }
  
  #close file
  ncdf4::nc_close(nc)
} #pool_ic_list2netcdf
