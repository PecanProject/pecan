#' Make some values into an NCDF dimension variable
#'
#' Units and longnames are looked up from the \code{\link{standard_vars}} table
#' @export
#'
#' @param dimname character vector, standard dimension name (must be in PEcAn.utils::standard_vars)
#' @param vals values of dimension; can be single value or vector
#' @return ncdim defined according to standard_vars
#' @author Anne Thomas
to_ncdim <- function(dimname,vals){
  dim <- PEcAn.utils::standard_vars[which(PEcAn.utils::standard_vars$Variable.Name == dimname),]
  #check dim exists
  if(nrow(dim) == 0){
    PEcAn.logger::logger.severe(paste("Dimension",dimname,"not in standard_vars"))
  }
  if(dim$Category != "Dimension"){
    PEcAn.logger::logger.severe(paste(dimname,"not a dimension or is deprecated"))
  }
  
  if(is.null(vals) || length(vals) == 0){
    PEcAn.logger::logger.severe(paste("Missing vals for dim",dimname,",please check input")) 
  } #not sure if this check is necessary
  
  units <- as.character(dim$Units) #if the units are a factor the function fails
  longname <- as.character(dim$Long.name)
  
  ncdim <- ncdf4::ncdim_def(name = dimname, vals = vals, units = units, longname = longname,-999) 
  
  return(ncdim)
} #to_ncdim


#' Define an NCDF variable
#'
#' @export
#'
#' @param varname character vector, standard variable name (must be in PEcAn.utils::standard_vars)
#' @param dims list of previously defined ncdims (function will match subset of dims for this variable in standard_vars; can include other dims--enables lapply.)
#' @return ncvar defined according to standard_vars
#' @author Anne Thomas
to_ncvar <- function(varname,dims){
  nc_var <- PEcAn.utils::standard_vars[which(PEcAn.utils::standard_vars$Variable.Name == varname),]
  #check nc_var exists
  if(nrow(nc_var)==0){
    PEcAn.logger::logger.severe(paste("Variable",varname,"not in standard_vars"))
  }
  
  dimset <- nc_var[,c("dim1","dim2","dim3","dim4")]
  dim <- dims[which(names(dims) %in% dimset)] #subset list of all dims for this variable
  #check that dim isn't 0
  if(length(dim)==0 || is.null(dim)){
    PEcAn.logger::logger.severe(paste("No dimensions were loaded for",varname))
  }
  
  units = as.character(nc_var$Units) #if the units are a factor the function fails
  longname <- as.character(nc_var$Long.name)
  
  ncvar <- ncdf4::ncvar_def(name = varname, units = units, longname = longname, dim = dim, -999, prec = "double") 
  
  return(ncvar)
} #to_ncvar
