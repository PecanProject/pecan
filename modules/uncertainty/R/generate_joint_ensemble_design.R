
#' Generate joint ensemble design for parameter sampling
#' Creates a joint ensemble design that maintains parameter correlations across
#' all sites in a multi-site run. This function generates sample indices that are shared across sites to ensure consistent parameter sampling.
#'
##' @param settings A PEcAn settings object containing ensemble configuration
##' @param sobol for activating sobol
##' @param ensemble_size Integer specifying the number of ensemble members
##' @return  A list containing ensemble samples and indices
##' 
##' @export

generate_joint_ensemble_design <- function(settings, ensemble_size, sobol = FALSE) {
  
  if(sobol){ ensemble_size = as.numeric(ensemble_size)*2 }
  
  ens.sample.method <- settings$ensemble$samplingspace$parameters$method
  design_list <- list()
  sampled_inputs <- list()
  posterior.files = rep(NA, length(settings$pfts))
  samp <- settings$ensemble$samplingspace
  parents <- lapply(samp, '[[', 'parent')
  order <- names(samp)[lapply(parents, function(tr) which(names(samp) %in% tr)) %>% unlist()]
  samp.ordered <- samp[c(order, names(samp)[!(names(samp) %in% order)])]
  
  for (i in seq_along(samp.ordered)) {
    input_tag <- names(samp.ordered)[i]
    parent_name <- samp.ordered[[i]]$parent
    
    parent_ids <- if (!is.null(parent_name)) sampled_inputs[[parent_name]] else NULL
    
    input_result <- PEcAn.uncertainty::input.ens.gen(
      settings = settings,
      ensemble_size = ensemble_size,
      input = input_tag,
      method = samp.ordered[[i]]$method,
      parent_ids = parent_ids
    )
    
    sampled_inputs[[input_tag]] <- input_result$ids
    design_list[[input_tag]] <- input_result$ids
  }
  
  # Sample parameters
  PEcAn.uncertainty::get.parameter.samples(settings,ensemble.size = ensemble_size, posterior.files, ens.sample.method)
  
  # Load samples from file
  samples.file <- file.path(settings$outdir, "samples.Rdata")
  samples <- new.env()
  if (file.exists(samples.file)) {
    load(samples.file, envir = samples)
    if (!is.null(samples$ensemble.samples)) {
      # Just a placeholder: extract representative trait index per ensemble member
      # You may want to flatten or select indices per trait
      design_list[["param"]] <- seq_len(ensemble_size)
    } else {
      PEcAn.logger::logger.warn("ensemble.samples not found in samples.Rdata")
    }
  } else {
    PEcAn.logger::logger.error(samples.file, "not found, this file is required")
  }
  design_matrix<- data.frame(design_list)
  
  if(sobol){
    half<-floor(ensemble_size / 2)
    X1 <- design_matrix[1:half, ]    
    X2 <- design_matrix[(half + 1):ensemble_size, ] 
    sobol_obj <- sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2)
    return(sobol_obj)
  }
  
  
  
  return(list(X=design_matrix))
}
