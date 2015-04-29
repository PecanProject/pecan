##' @title Format ring & plot data for JAGA
##' @name  buildJAGSdata_InventoryRings
##' @param combined  object returned from matchInventoryRings. Matrix with both increment and plot data
##' @param inc.unit.conv  conversion factor from loaded increments to cm (of radius)
##' @return list
##' @author Michael Dietze
##' @description builds the JAGS data object for the tree ring / inventory fusion code
##' also sets all the priors
##' @export
buildJAGSdata_InventoryRings <- function(combined,inc.unit.conv = 0.1){
  
  ## pull out growth to a matrix, convert to cm of diameter
  y = as.matrix(combined[,!is.na(as.numeric(colnames(combined)))])*inc.unit.conv*2
  time = as.numeric(colnames(y))
  
  ## pull out diameter to a matrix
  DBH.cols = grep("DBH",colnames(combined))
  DBH = as.matrix(combined[,DBH.cols])
  class(DBH) <- "numeric"
  z = matrix(NA,nrow(y),ncol(y))
  DBH.years = as.numeric(sub("DBH","",colnames(combined)[DBH.cols]))
  DBH.years = ifelse(DBH.years < 20,DBH.years+2000,DBH.years+1900)
  z[,which(time %in% DBH.years)] = DBH
  
  ## build data object for JAGS
  n = nrow(y)
  data <- list(y=y[1:n,],z = z[1:n,],ni=n,nt=ncol(y),x_ic=1,tau_ic=0.0001,
               a_dbh=16,r_dbh=8,a_inc=0.001,r_inc=1,a_add=1,r_add=1,time=time)
  
  return(data)
  
}