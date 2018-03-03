##' Paramater Data Assimilation using emulator on multiple sites in three modes: local, global, hierarchical
##' First draft, not complete yet
##'
##' @title Paramater Data Assimilation using emulator on multiple sites
##' @param multi.settings = a pecan multi-settings list
##'
##' @return settings
##'
##' @author Istem Fer
##' @export
pda.emulator.ms <- function(multi.settings) {
  
  ## -------------------------------------- Initialization --------------------------------------------------- 
 
  # check mode 
  pda.mode <- unique(sapply(multi.settings$assim.batch,`[[`, "mode"))
  
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
  
  # how many sites
  nsites <- length(multi.settings)

  
  # lists to collect emulators and run MCMC per site later
  SS.stack      <- vector("list", nsites) 
  gp.stack      <- vector("list", nsites) 
  prior.stack   <- vector("list", nsites) 
  nstack        <- vector("list", nsites) 
  
  ## -------------------------------------- Runs and build emulator ------------------------------------------
  
  for(s in seq_along(multi.settings)){ # site runs - loop begin
    
    settings <- multi.settings[[s]]
    
  } # site runs - loop end
  
}