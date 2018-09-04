##' @title Contruct.Pf
##' @name  Contruct.Pf
##' @author Hamze Dokoohaki
##' 
##' @param site.ids a vector name of site ids  
##' @param var.names vector names of state variable names
##' @param X a matrix of state variables. In this matrix rows represent ensembles, while columns show the variables for different sites.
##' @param localization.FUN This is function that performs the localization of the Pf matrix and it returns a localized matrix with the same dimensions.
##' @description The argument X needs to have an atrribute pointing the state variables to their corresponding site. This atrribute needs to be called `Site`.
##' At the moment, the cov between state variables at block defining the cov between two sites are assumed zero.
##' @return It returns the var-cov matrix of state variables at multiple sites.
##' @export

Contruct.Pf <- function(site.ids, var.names, X, localization.FUN=NULL,...) {
  
  nsite <- length(site.ids)
  nvariable <- length(var.names)
  # I will make a big cov matrix and then I will populate it withgit status cov of each site
  pf.matrix <-matrix(0,(nsite*nvariable),(nsite*nvariable))
  
  ## This makes the diagnol of our big matrix - first filters out each site, estimates the cov and puts it where it needs to go.
  for (site in site.ids){
    #let's find out where this cov (for the current site needs to go in the main cov matrix)
    pos.in.matrix <- which(attr(X,"Site") %in% site)
   #forach site let's get the Xs
    pf.matrix [pos.in.matrix, pos.in.matrix] <- cov( X [, pos.in.matrix] )
  }
  
  
  # This is where we estimate the cov between state variables of different sites
  #I put this into a sperate loop so we can have more control over it
  site.cov.orders <- expand.grid(site.ids,site.ids) %>% filter( Var1 != Var2)
  
  for (i in 1:nrow(site.cov.orders)){
    # first we need to find out where to put it in the big matrix
    rows.in.matrix <- which(attr(X,"Site") %in% site.cov.orders[i,1])
    cols.in.matrix <- which(attr(X,"Site") %in% site.cov.orders[i,2])
    #estimated between these two sites
    two.site.cov <- cov( X [, c(rows.in.matrix, cols.in.matrix)] )[(nvariable+1):(2*nvariable),1:nvariable]
    # this is something we can pplay around with - I'm setting the off diag to zero 
    two.site.cov [which(lower.tri(two.site.cov, diag = FALSE),T) %>% rbind (which(upper.tri(two.site.cov,F),T))] <- 0
  
    #putting it back to the main matrix
    pf.matrix [rows.in.matrix, cols.in.matrix] <- two.site.cov
    
  }
  # if I see that there is a localization function passed to this - I run it by the function.
  if (!is.null(localization.FUN)) pf.matrix <- localization.FUN (pf.matrix,...)
  
  return(pf.matrix)

}

##' @title Construct.R
##' @name  Construct.R
##' @author Hamze Dokoohaki
##' 
##' @param site.ids a vector name of site ids  
##' @param var.names vector names of state variable names
##' @param obs.t.mean list of vector of means for the time t for different sites.
##' @param obs.t.cov list of list of cov for the time for different sites.
##’  
##' 
##' @description Make sure that both lists are named with siteids.
##' 
##' @return This function returns a list with Y and R ready to be sent to the analysis functions.
##' @export

Construct.R<-function(site.ids, var.names, obs.t.mean, obs.t.cov){

  Y<-c()
  nsite <- length(site.ids)
  nvariable <- length(var.names)
  # I will make a big cov matrixand then I will populate it when cov of each site
  R <-matrix(0,(nsite*nvariable),(nsite*nvariable))
  
  for (site in site.ids){
   
    choose <- sapply(var.names, agrep, x=names(obs.t.mean[[site]]), max=1, USE.NAMES = F) %>% unlist
    #forach site let's get the obs
    Y <- c(Y, unlist(obs.t.mean[[site]][choose]))
    pos <- which(site.ids %in% site)
    startp <- (pos-1)*(nvariable)+1
    endp <- startp + nvariable - 1
    
    R.site<- as.matrix(obs.t.cov[[site]][choose,choose])
    R.site[is.na(R.site)]<-0
   
    R[startp:endp, startp:endp] <- R.site
  }

  return(list(Y=Y, R=R))
}



Create_blocked_matrix <- function(nsite, nvar, Matrix.value.diag=0,offdiag=NA){
  outmatrix <- matrix(offdiag, nsite*nvar, nsite*nvar )
  positions <- seq(1,(nsite*nvar),by=nvar) 
  for (pos in positions){
    outmatrix[pos:(pos+nvar-1), pos:(pos+nvar-1)] <- matrix(Matrix.value.diag,nvar, nvar)
  }
  outmatrix
  
}
