#' veg2model.SIPNET 
#' @name veg2model.SIPNET
#' @title veg2model.SIPNET
#' @param input input object taken from cohort2pool
#' @param outdir location where you want ncdf files stored
#' @param siteid BETY site id 
#' @param ens number of ensemble members
#'
#' @return result object with filepaths to ncdf files
#' @export
#' @author Alexis Helgeson
#' 
veg2model.SIPNET <- function(input, outdir, siteid, ens){
# Execute pool_ic function
result <- PEcAn.data.land::pool_ic_list2netcdf(input = input, outdir = outdir, siteid = siteid, ens = ens)

return(result)
}