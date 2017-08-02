##' TDM Save Betas
##' Saves betas that are calculated during gen.subdaily.models()
# ----------------------------------- 
# Description
# -----------------------------------
##' @title save.betas
##' @family tdm - Temporally Downscale Meteorology
##' @author Christy Rollinson, James Simkins
##' @description Function to save betas as a .nc file. This is utilized in
##'              gen.subdaily.models() when linear regression models are created
# ----------------------------------- 
# Parameters
# -----------------------------------
##' @param model.out list linear regression model output
##' @param betas name of the layer of betas to save (e.g. 'betas' or 'betas.resid')
##' @param outfile location where output will be stored
##' @export
# -----------------------------------
#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------
save.betas <- function(model.out, betas, outfile) {
  
  var.list <- list()
  for (v in names(model.out)) {
    # Note: Need a separate list of coefficients for each variable
    dimY <- ncdf4::ncdim_def(paste0("coeffs_", v), units = "unitless", 
                             longname = "model.out coefficients", vals = 1:ncol(model.out[[v]][[betas]]))
    dimX <- ncdf4::ncdim_def("random", units = "unitless", longname = "random betas", 
                             vals = 1:nrow(model.out[[v]][[betas]]))
    
    var.list[[v]] <- ncdf4::ncvar_def(v, units = "coefficients", dim = list(dimX, 
                                                                            dimY), longname = paste0("day ", v, " model.out coefficients"))
  }
  
  nc <- ncdf4::nc_create(outfile, var.list)
  for (v in names(model.out)) {
    ncdf4::ncvar_put(nc, var.list[[v]], model.out[[v]][[betas]])
  }
  ncdf4::nc_close(nc)
  
  
}