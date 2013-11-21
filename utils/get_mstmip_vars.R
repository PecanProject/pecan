ncvar_def.pecan <- function(name, units, longname,
                            dim = list(lon, lat, t),
                            missval = -999, prec = "float"){
  ans <- ncvar_def(name = name, units = units, dim = dim,
                   missval = missval, prec = "float")
  return(ans)
}

make.ncdf_vars <- function(vars = c("LAI")){
  require(data.table)
  require(ncdf4)
  data(mstmip_vars, package = "PEcAn.utils")
  mstmip <- data.table(mstmip_vars)
  mstmip[Variable.Name %in% vars,
         list(name = Variable.Name, units = Units,
                          longname = Long.name)]
  
  with(mstmip_variables,
       (Variable.Name, function(x)
  names(lapply(mstmip_variables$Variable.Name, identity))
}
