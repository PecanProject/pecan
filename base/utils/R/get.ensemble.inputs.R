## split clim file into smaller time units to use in KF

#' get.ensemble.inputs
#'
#' Splits climate met for SIPNET
#'
#' @author Mike Dietze and Ann Raiho
#'
#' @param settings PEcAn settings list
#' @param ens ensemble number. default = 1
#'
#' @return find correct ensemble inputs
#' @export

get.ensemble.inputs <- function(settings, ens = 1){
  
  ##grab all inputs for this ensemble member
  inputs <- list()
  input.list <- names(settings$run$inputs)
  input.table <- table(input.list)
  
  ##loop over inputs to get the correct inputs for each type
  for(i in seq_along(input.table)){
    sel <- which(input.list == names(input.table)[i])
    inputs[[i]] <- settings$run$inputs[[(ens-1) %% input.table[i] + 1]]
    names(inputs)[i] <- names(input.table)[i]
  }
  
  return(inputs)
}
