## builds the JAGS data object for the tree ring / inventory fusion code
## also sets all the priors
buildJAGSdata_InventoryRings <- function(combined,inc.unit.conv = 0.1){
  
  ## pull out growth to a matrix, convert to cm of diameter
  y = as.matrix(combined[,!is.na(as.numeric(colnames(combined)))])*inc.unit.conv*2
  time = as.numeric(colnames(y))
  
  ## pull out diameter to a matrix
  DBH = as.matrix(combined[,grep("DBH",colnames(combined))])
  class(DBH) <- "numeric"
  z = matrix(NA,nrow(y),ncol(y))
  DBH.years = as.numeric(sub("DBH","",colnames(DBH)))
  DBH.years = ifelse(DBH.years < 20,DBH.years+2000,DBH.years+1900)
  z[,which(time %in% DBH.years)] = DBH
  
  ## build data object for JAGS
  n = nrow(y)
  data <- list(y=y[1:n,],z = z[1:n,],ni=n,nt=ncol(y),x_ic=1,tau_ic=0.000001,
               a_dbh=8,r_dbh=4,a_inc=1,r_inc=0.01,a_add=1,r_add=1)
  
  return(data)
  
}