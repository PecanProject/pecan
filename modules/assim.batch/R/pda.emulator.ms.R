##' Paramater Data Assimilation using emulator on multiple sites in three modes: local, global, hierarchical
##' First draft, not complete yet
##'
##' @title Paramater Data Assimilation using emulator on multiple sites
##' @param settings = a pecan settings list
##'
##' @return settings
##'
##' @author Istem Fer
##' @export
pda.emulator.ms <- function(settings, external.priors = NULL, params.id = NULL, param.names = NULL, prior.id = NULL, 
                            chain = NULL, iter = NULL, adapt = NULL, adj.min = NULL, 
                            ar.target = NULL, jvar = NULL, n.knot = NULL) {
  
  ## this bit of code is useful for defining the variables passed to this function if you are
  ## debugging
  if (FALSE) {
    params.id <- param.names <- prior.id <- chain <- iter <- NULL
    n.knot <- adapt <- adj.min <- ar.target <- jvar <- external.priors <- NULL
  }
  
  
  # check mode 
  pda.mode <- settings$assim.batch$mode
  
  # how many sites
  nsites <- length(settings)
  
  if(pda.mode == "local"){
    local <- TRUE
    global <- hierarchical <- FALSE
  }else if(pda.mode == "global"){
    global <- TRUE
    local <- hierarchical <- FALSE
  }else if(pda.mode == "hierarchical"){
    hieararchical <- TRUE
    local <- global <- FALSE
  }else{
    local <- global <- hierarchical <- TRUE
  }
  
  
  
  
  
  
  
  
}