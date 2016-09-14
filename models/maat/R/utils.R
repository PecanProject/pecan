## Helper functions for the MAAT model

##
## Convert ED2 met to MAAT
# ed2maat.met <- function(in.ed2.met = NULL, out.dir = NULL, out.csv = NULL){
#   
#   # Function to extract hdf5 data
#   getHdf5Data <- function(nc, var) {
#     if(var %in% names(nc$var)) {
#       return(ncvar_get(nc, var))
#     } else {
#       logger.warn("Could not find", var, "in ed hdf5 output.")
#       return(-999)
#     }
#   }
#   
#   conversion <- function(col, mult) {
#     ## make sure only to convert those values that are not -999
#     out[[col]][out[[col]] != -999] <- out[[col]][out[[col]] != -999] * mult
#     return(out)
#   }
#   
#   checkTemp <- function(col) {
#     out[[col]][out[[col]] == 0] <- -999
#     return(out)
#   }
#   
#   
#   hdf5.file <- nc_open(in.ed2.met)
#   
#   
# 
# }

