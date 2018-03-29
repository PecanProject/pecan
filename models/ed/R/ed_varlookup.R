#' Lookup function for translating commonly used ED variables
#' returns out list, readvar variables to read from file, expr if any derivation is needed
#' @export
ed.var <- function(varname) {
  if(varname == "AGB") {
    out = list(readvar = "AGB_CO",   
               expr    = "AGB_CO")                                                  } else
  if(varname == "BA") {
    out = list(readvar = "BA_CO",
               expr    = "BA_CO")                                                   } else
  if(varname == "DBH") {
    out = list(readvar  = "DBH",
               expr     = "DBH")                                                    } else
  if(varname == "AbvGrndWood") {
    out = list(readvar  = c("AGB_CO", "BALIVE"),
               expr     = "AGB_CO-BALIVE")                                          } else
  if(varname == "GWBI") {
    out = list(readvar  = "DAGB_DT", # this is actually rate of change in AGB
               expr     = "DAGB_DT")                                                } else
  { # No Match!
    warning(paste0("Couldn't find varname ", varname, "!"))
    out = NULL
  }
  return(out)
}
