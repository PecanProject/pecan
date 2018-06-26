#' Lookup function for translating commonly used ED variables
#' returns out list, readvar variables to read from file, expr if any derivation is needed
#' @export
ed.var <- function(varname) {
  if(varname == "AGB") {
    out = list(readvar = "AGB_CO", 
               type = 'co', units = "kgC/plant", 
               drelated = NULL,                    # other deterministically related vars?
               expr    = "TOTAL_AGB")                                                 
  } else if(varname == "TotLivBiom") {
    out = list(readvar = c("BALIVE"),
               type = 'co', units = "kgC/plant", 
               drelated = NULL,
               expr    = "BALIVE")                                                 
  } else if(varname == "BA") {
    out = list(readvar = "BA_CO",
               type = 'co', units = "cm2/plant", 
               drelated = NULL,
               expr    = "BA_CO")                                                  
  } else if(varname == "DBH") {
    out = list(readvar = "DBH",
               type = 'co', units = "cm/plant",
               drelated = NULL,
               expr    = "DBH")                                                    
  } else if(varname == "AbvGrndWood") {
    out = list(readvar = c("TOTAL_AGB"), 
               type = 'grid', units = "kgC/m2", 
               drelated = NULL, 
               expr    = "TOTAL_AGB")                                           
  } else if(varname == "leaf_carbon_content") {
    out = list(readvar = "BLEAF", 
               type = 'co', units = "kgC/plant", 
               drelated = NULL, 
               expr    = "BLEAF")                                                  
  } else if(varname == "root_carbon_content") {
    out = list(readvar = "BROOT", 
               type = 'co', units = "kgC/plant", 
               drelated = NULL, 
               expr    = "BROOT")                                                  
  } else if(varname == "GWBI") {
    out = list(readvar = "TOTAL_AGB_GROWTH", # this is actually rate of change in AGB 
               type = 'grid', units = "kgC/m2/yr", 
               drelated = NULL,
               expr    = "TOTAL_AGB_GROWTH")                                              
  } else { # No Match!
    warning(paste0("Couldn't find varname ", varname, "!"))
    out = NULL
  }
  return(out)
}
