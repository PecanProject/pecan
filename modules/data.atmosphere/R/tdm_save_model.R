##' TDM Save Models
##' Saves models that are created during gen.subdaily.models()
# ----------------------------------- 
# Description
# -----------------------------------
##' @title save.model
##' @family tdm - Temporally Downscale Meteorology
##' @author Christy Rollinson, James Simkins
##' @description Function to save models as a .nc file. This is utilized in
##'              gen.subdaily.models() when linear regression models are created
# ----------------------------------- 
# Parameters
# -----------------------------------
##' @param model.out list linear regression model output
##' @param model name of the layer of model to save (e.g. 'model' or 'model.resid')
##' @param outfile location where output will be stored
##' @export
# -----------------------------------
#----------------------------------------------------------------------
# Begin Function
#----------------------------------------------------------------------
save.model <- function(model.out, model, outfile) {

  mod.list <- list()
  for (v in names(model.out)) {
    mod.list[[v]] <- model.out[[v]][[model]]
  }
  
  save(mod.list, file = outfile)
}

