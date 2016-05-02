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
  
  ## if present, pull out mortality and recruitment
  COND.cols = grep("COND",colnames(combined))
  if(length(COND.cols)>0){
    COND = as.matrix(combined[,COND.cols])
    w = matrix(NA,nrow(y),ncol(y))
    COND.years = as.numeric(sub("COND","",colnames(combined)[COND.cols]))
    COND.years = ifelse(COND.years < 20,COND.years+2000,ifelse(COND.years < 100,COND.years+1900,COND.years))
    w[,which(time %in% COND.years)] = COND
    ## convert COND matrix to numeric 0/1
    w[w=="L"] = 1
    w[w=='miss'] = NA
    for(i in 1:nrow(w)){
      ## recruitment
      r = which(w[i,] %in% 'R')
      if(length(r)>0){
        w[i,seq_len(r-1)] = 0  ## should really set last census, not last year, to 0, and then put NAs in between  ****
        w[i,r] = 1
      }
      ## mortality
      m = which(w[i,] %in% 'M')
      if(length(m)>0){
        w[i,m:ncol(w)] = 0
      }
      ## known to be alive
      l = which(w[i,] %in% '1')
      if(length(l)>1){
        
      }
    }
    class(w) <- "numeric"
    ## Fill in COND matrix
    

  } else {
    w = matrix(1,nrow(y),ncol(y))
  }
  
  ## build data object for JAGS
  n = nrow(y)
  data <- list(y=y[1:n,],z = z[1:n,],ni=n,nt=ncol(y),x_ic=1,tau_ic=0.0001,
               a_dbh=16,r_dbh=8,a_inc=0.001,r_inc=1,a_add=1,r_add=1,time=time)
  
  return(data)
  
}