#' veg2model.SIPNET
#' 
#' @param outfolder location to store ncdf files
#' @param poolinfo object passed from write_ic contains output from cohort2pool function
#' @param siteid object passed from write_ic contains site id
#' @param ens number of ensemble members
#' 
#' @return result object with filepaths to ncdf files
#' @export
#' @author Alexis Helgeson
#' 
veg2model.SIPNET <- function(outfolder, poolinfo, siteid, ens){

  # Execute pool_ic function
result <- PEcAn.data.land::pool_ic_list2netcdf(input = poolinfo, outdir = outfolder, siteid = siteid, ens = ens)

return(result)
}