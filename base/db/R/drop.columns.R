drop.columns <- function(data, columns){
  return(data[, which(!colnames(data) %in% columns)])
}
##=============================================================================#
