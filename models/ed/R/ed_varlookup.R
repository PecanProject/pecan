#' Lookup function for translating commonly used ED variables
#' returns out list, readvar variables to read from file, expr if any derivation is needed
#' @export
ed.var <- function(varname) {
  if(varname == "Total aboveground biomass") {
    out = list(readvar = "AGB_CO",   
               expr    = "AGB_CO")                                                  } else
  if(varname == "Basal area") {
    out = list(readvar = "BA_CO",
               expr    = "BA_CO")                                                   } else
  if(varname == "Diameter at Breast Height") {
    out = list(readvar  = "DBH",
               expr     = "DBH")                                                    } else
  if(varname == "Above ground woody biomass") {
    out = list(readvar  = c("AGB_CO", "BALIVE"),
               expr     = "AGB_CO - BALIVE")                                        } else
  { # No Match!
    warning(paste0("Couldn't find varname ", varname, "!"))
    out = NULL
  }
  return(out)
}
