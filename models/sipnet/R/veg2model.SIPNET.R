#' veg2model.SIPNET 
#' @name veg2model.SIPNET
#' @title veg2model.SIPNET
#'
#' @param ens number of ensemble members
#' @param outfolder location to store ncdf files
#' @param poolinfo object passed from write_ic contains output from cohort2pool function
#' @param new_site object passed from write_ic contains site id
#' @param source object passed from write_ic
#'
#' @return result object with filepaths to ncdf files
#' @export
#' @author Alexis Helgeson
#' 
veg2model.SIPNET <- function(outfolder, poolinfo, new_site, ens){

  outdir <- outfolder
  siteid <- new_site$id
  # Execute pool_ic function
result <- PEcAn.data.land::pool_ic_list2netcdf(input = input, outdir = outdir, siteid = siteid, ens = ens)

return(result)
}