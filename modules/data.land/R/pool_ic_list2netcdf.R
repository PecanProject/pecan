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
  
  standard_vars <- read.csv(system.file("data/standard_vars.csv",package="PEcAn.utils"),stringsAsFactors = FALSE)
  
  ###function to lapply to all dimensions in input list and return a list of ncdims
  to_ncdim <- function(dimname,standard_vars,input){
    dim <- standard_vars[which(standard_vars$Variable.Name == dimname),]
    #check dim exists
    if(nrow(dim)==0){
      PEcAn.utils::logger.severe(paste("Dimension",dimname,"not in standard_vars"))
    }
    if(dim$Category != "Dimension"){
      PEcAn.utils::logger.severe(paste(dimname,"not a dimension or is deprecated"))
    }
    
    vals = input$dims[[which(names(input$dims)==dimname)]] #makes function non-modular but works assuming it's only used in pool_ic_list2netcdf
    if(is.null(vals) || length(vals)==0){
      PEcAn.utils::logger.severe(paste("Missing vals for dim",dimname,",please check input")) #don't know if this could happen without explicit NULL in input
    }
    
    units = as.character(dim$Units) #if the units are a factor the function fails
    longname <- as.character(dim$Long.name)
    
    ncdim <- ncdf4::ncdim_def(name = dimname, vals = vals, units = units, longname = longname,-999) 
    
    return(ncdim)
  }
  
  dims <- lapply(names(input$dims),to_ncdim,standard_vars,input)
  names(dims) <- names(input$dims)
  
  
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
  
  ncvars <- lapply(names(input$vals),to_ncvar,standard_vars,dims)
  
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
