##' @title Contruct.Pf
##' @name  Contruct.Pf
##' @author Hamze Dokoohaki
##' 
##' @param site.ids a vector name of site ids.  
##' @param var.names vector names of state variable names.
##' @param X a matrix of state variables. In this matrix rows represent ensembles, while columns show the variables for different sites.
##' @param localization.FUN This is the function that performs the localization of the Pf matrix and it returns a localized matrix with the same dimensions.
##' @description The argument X needs to have an attribute pointing the state variables to their corresponding site. This attribute needs to be called `Site`.
##' At the moment, the cov between state variables at blocks defining the cov between two sites are assumed zero.
##' @return It returns the var-cov matrix of state variables at multiple sites.
##' @export 


Contruct.Pf <- function(site.ids, var.names, X, localization.FUN=NULL, t=1, blocked.dis=NULL, ...) {
  #setup
  nsite <- length(site.ids)
  nvariable <- length(var.names)
  # I will make a big cov matrix and then I will populate it with the cov of each site
  pf.matrix <-matrix(0,(nsite*nvariable),(nsite*nvariable))
  
  ## This makes the diagonal of our big matrix - first filters out each site, estimates the cov and puts it where it needs to go.
  for (site in site.ids){
    #let's find out where this cov (for the current site needs to go in the main cov matrix)
    pos.in.matrix <- which(attr(X,"Site") %in% site)
   #foreach site let's get the Xs
    pf.matrix [pos.in.matrix, pos.in.matrix] <- cov( X [, pos.in.matrix] ,use="complete.obs")
  }
  
  # This is where we estimate the cov between state variables of different sites
  #I put this into a sperate loop so we can have more control over it
  site.cov.orders <- expand.grid(site.ids,site.ids) %>%
                        filter( Var1 != Var2)

  for (i in 1:nrow(site.cov.orders)){
    # first we need to find out where to put it in the big matrix
    rows.in.matrix <- which(attr(X,"Site") %in% site.cov.orders[i,1])
    cols.in.matrix <- which(attr(X,"Site") %in% site.cov.orders[i,2])
    #estimated between these two sites
    two.site.cov <- cov( X [, c(rows.in.matrix, cols.in.matrix)],use="complete.obs" )[(nvariable+1):(2*nvariable),1:nvariable]
    # I'm setting the off diag to zero 
    two.site.cov [which(lower.tri(two.site.cov, diag = FALSE),T) %>% rbind (which(upper.tri(two.site.cov,F),T))] <- 0
    #putting it back to the main matrix
    pf.matrix [rows.in.matrix, cols.in.matrix] <- two.site.cov
  }
  
  # if I see that there is a localization function passed to this - I run it by the function.
  if (!is.null(localization.FUN)) {
    pf.matrix.out <- localization.FUN (pf.matrix, blocked.dis, ...)
  } else{
    pf.matrix.out <- pf.matrix
  }
  
  # adding labels to rownames and colnames
  labelss <- paste0(rep(var.names, length(site.ids)) %>% as.character(),"(",
         rep(site.ids, each=length(var.names)),")") 
  
  colnames(pf.matrix.out ) <-labelss
  rownames(pf.matrix.out ) <-labelss
  
  return(pf.matrix.out)

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

  # keeps Hs of sites
  site.specific.Rs <-list()
  #
  nsite <- length(site.ids)
  #
  nvariable <- length(var.names)
  Y<-c()
  
  for (site in site.ids){
    choose <- sapply(var.names, agrep, x=names(obs.t.mean[[site]]), max=1, USE.NAMES = F) %>% unlist
    # if there is no obs for this site
    if(length(choose)==0){
      next;
    }else{
      Y <- c(Y, unlist(obs.t.mean[[site]][choose]))
      # collecting them
      site.specific.Rs <- c(site.specific.Rs, list(as.matrix(obs.t.cov[[site]][choose,choose])) )
    }
  #make block matrix out of our collection
  R <- Matrix::bdiag(site.specific.Rs) %>% as.matrix()
    }

  return(list(Y=Y, R=R))
}


##' @title block_matrix
##' @name  block_matrix
##' @author Guy J. Abel
##' 
##' @param x Vector of numbers to identify each block.
##' @param b Numeric value for the size of the blocks within the matrix ordered depending on byrow
##' @param byrow logical value. If FALSE (the default) the blocks are filled by columns, otherwise the blocks in the matrix are filled by rows.
##' @param dimnames Character string of name attribute for the basis of the blcok matrix. If NULL a vector of the same length of b provides the basis of row and column names.#'.
##’  
##' 
##' @description This function is adopted from migest package.
##' 
##' @return Returns a matrix with block sizes determined by the b argument. Each block is filled with the same value taken from x.
##' @export
block_matrix <- function (x = NULL, b = NULL, byrow = FALSE, dimnames = NULL) {
  n <- length(b)
  bb <- rep(1:n, times = b)
  dn <- NULL
  if (is.null(dimnames)) {
    dn <- rep(1:n, times = b)
    dd <- unlist(sapply(b, seq, from = 1))
    dn <- paste0(dn, dd)
    dn <- list(dn, dn)
  }
  if (!is.null(dimnames)) {
    dn <- dimnames
  }
  xx <- matrix(NA, nrow = sum(b), ncol = sum(b), dimnames = dn)
  k <- 1
  if (byrow == TRUE) {
    for (i in 1:n) {
      for (j in 1:n) {
        xx[i == bb, j == bb] <- x[k]
        k <- k + 1
      }
    }
  }
  if (byrow == FALSE) {
    for (j in 1:n) {
      for (i in 1:n) {
        xx[i == bb, j == bb] <- x[k]
        k <- k + 1
      }
    }
  }
  return(xx)
}

##' @title Construct.H.multisite
##' @name  Construct.H.multisite
##' @author Hamze
##' 
##' @param site.ids a vector name of site ids  
##' @param var.names vector names of state variable names
##' @param obs.t.mean list of vector of means for the time t for different sites. 
##' 
##' @description This function is makes the blocked mapping function.
##' 
##' @return Returns a matrix with block sizes determined by the b argument. Each block is filled with the same value taken from x.
##' @export
Construct.H.multisite <- function(site.ids, var.names, obs.t.mean){

  site.ids.with.data <- names(obs.t.mean)
  site.specific.Hs <- list()
  
  
  nsite <- length(site.ids) # number of sites
  nsite.ids.with.data <-length(site.ids.with.data) # number of sites with data
  nvariable <- length(var.names)
  #This is used inside the loop below for moving between the sites when populating the big H matrix
  nobs <- obs.t.mean %>% map_dbl(~length(.x)) %>% max # this gives me the max number of obs at sites
  nobstotal<-obs.t.mean %>% purrr::flatten() %>% length() # this gives me the total number of obs
  #H <- matrix(0, (nobs * nsite.ids.with.data), (nvariable*nsite))
  #big empty H which needs to be filled in.
  #Having the total number of obs as the row number
  H <- matrix(0,  nobstotal, (nvariable*nsite))
  j<-1
  for(i in seq_along(site.ids)){
    
    site <- site.ids[i]
    obs.names <- names(obs.t.mean[[site]])
    
    if(is.null(obs.names)) next;
    
    choose.col <- sapply(obs.names, agrep, x = var.names, max = 1, USE.NAMES = F) %>% unlist
    choose.row <- sapply(var.names, agrep, x = obs.names, max = 1, USE.NAMES = F) %>% unlist
    
    # empty matrix for this site
    H.this.site <- matrix(0, length(choose), nvariable)
    # fill in the ones based on choose
    H.this.site [choose.row, choose.col] <- 1
    
    pos.row<- ((nobs*j)-(nobs-1)):(nobs*j)
    pos.col<- ((nvariable*i)-(nvariable-1)):(nvariable*i)
    
    H[pos.row,pos.col] <-H.this.site
    
    j <- j +1
  }
  
  return(H)
}
