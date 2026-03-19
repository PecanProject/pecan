#Function to run across all grid cells/years
calc_curve <- function(id_i, df, method = "Beck"){
  #Filter the full dataset to this grid cell/year. Add explicit NAs
  x <- df %>%
    filter(id == id_i) %>%
    complete(img_doy = 1:365)
  
  #Run double log function
  if(method == "Beck"){
    fit <- FitDoubleLogBeck(x$evi, hessian = T, ninit = 100)
  } else if(method == "Elmore"){
    fit <- FitDoubleLogElmore(x$evi, hessian = T, ninit = 100)
  } else {
    stop("Method not recognized. Options are Beck and Elmore")
  }
  
  #Format output
  out <- data.frame(param_name = names(fit$params),
                    param_value = fit$params,
                    stdError = fit$stdError,
                    id = id_i)
  rownames(out) <- NULL
  
  return(out)
}