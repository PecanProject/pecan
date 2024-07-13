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
##' @importFrom rlang .data
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
    pf.matrix [pos.in.matrix, pos.in.matrix] <- stats::cov( X [, pos.in.matrix] ,use="complete.obs")
  }
  
  # This is where we estimate the cov between state variables of different sites
  #I put this into a sperate loop so we can have more control over it
  site.cov.orders <- expand.grid(site.ids,site.ids) %>%
    dplyr::filter( .data$Var1 != .data$Var2)

  for (i in seq_len(nrow(site.cov.orders))){
    # first we need to find out where to put it in the big matrix
    rows.in.matrix <- which(attr(X,"Site") %in% site.cov.orders[i,1])
    cols.in.matrix <- which(attr(X,"Site") %in% site.cov.orders[i,2])
    #estimated between these two sites
    two.site.cov <- stats::cov( X [, c(rows.in.matrix, cols.in.matrix)],use="complete.obs" )[(nvariable+1):(2*nvariable),1:nvariable]
    # I'm setting the off diag to zero 
    two.site.cov [which(lower.tri(two.site.cov, diag = FALSE),TRUE) %>% rbind (which(upper.tri(two.site.cov,FALSE),TRUE))] <- 0
    #putting it back to the main matrix
    pf.matrix [rows.in.matrix, cols.in.matrix] <- two.site.cov
  }
  
  # if I see that there is a localization function passed to this - I run it by the function.
  if (!is.null(localization.FUN) && nsite > 1) {
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
    choose <- sapply(var.names, agrep, x=names(obs.t.mean[[site]]), max=1, USE.NAMES = FALSE) %>% unlist
    # if there is no obs for this site
    if(length(choose) == 0){
      next;
    }else{
      Y <- c(Y, unlist(obs.t.mean[[site]][choose]))
      #collecting them
      if (ncol(obs.t.mean[[site]]) > 1)
      {
        site.specific.Rs <- c(site.specific.Rs, list(as.matrix(obs.t.cov[[site]][choose,choose])))
      } else {
        site.specific.Rs <- c(site.specific.Rs, list(as.matrix(obs.t.cov[[site]][choose])))
      }
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
##' @param dimnames Character string of name attribute for the basis of the block matrix. If NULL a vector of the same length of b provides the basis of row and column names.#'.
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
  #we first create a matrix containing site.ids, var.names, observations, and the index of observations across obs.mean.
  site.ids.matrix <- rep(site.ids, each = length(var.names))#this is replicated site.ids. The number of replication depends on how many vars in total.
  var.names.matrix <- rep(var.names, length(site.ids))#this is the state variable names from settings.
  H.pre.matrix <- data.frame(site.ids.matrix, var.names.matrix, NA, NA) %>% `colnames<-` (c("site.id", "var.name", "obs", "obs.ind"))
  obs.ind <- 1
  #loop over site.ids * var.names
  for (i in seq_along(site.ids.matrix)) {
    site.id <- H.pre.matrix[i,]$site.id
    var.name <- H.pre.matrix[i,]$var.name
    site.ind <- which(names(obs.t.mean)==site.id)
    if(length(site.ind) > 0){
      obs <- obs.t.mean[[site.ind]]
      var.ind <- which(names(obs)==var.name)
      if(length(var.ind) > 0){
        #write observation and the index into the matrix.
        H.pre.matrix[i,]$obs <- obs[[var.ind]]
        H.pre.matrix[i,]$obs.ind <- obs.ind
        obs.ind <- obs.ind + 1
      }
    }
  }
  #convert the matrix into H matrix.
  H <- matrix(0, max(H.pre.matrix$obs.ind, na.rm=T), dim(H.pre.matrix)[1])
  for (i in seq_along(site.ids.matrix)) {
    H[H.pre.matrix[i,]$obs.ind, i] <- 1
  }
  H
}

##' @title construct_nimble_H
##' @name  construct_nimble_H
##' @author Dongchen Zhang
##' 
##' @param site.ids a vector name of site ids  
##' @param var.names vector names of state variable names
##' @param obs.t list of vector of means for the time t for different sites.
##' @param pft.path physical path to the pft.csv file.
##' @param by criteria, it supports by variable, site, pft, all, and single Q.
##' 
##' @description This function is an upgrade to the Construct.H.multisite function which provides the index by different criteria.
##' 
##' @return Returns one vector containing index for which Q to be estimated for which variable, 
##' and the other vector gives which state variable has which observation (= element.W.Data).
##' @export
construct_nimble_H <- function(site.ids, var.names, obs.t, pft.path = NULL, by = "single"){
  if(by == "pft" | by == "block_pft_var" & is.null(pft.path)){
    PEcAn.logger::logger.info("please provide pft path.")
    return(0)
  }
  H <- Construct.H.multisite(site.ids, var.names, obs.t)
  if (by == "var") {
    total_var_name <- rep(var.names, length(site.ids))
    Ind <- rep(0, dim(H)[2])
    for (i in seq_along(var.names)) {
      Ind[which(total_var_name == var.names[i])] <- i
    }
  } else if (by == "site") {
    total_site_id <- rep(site.ids, each = length(var.names))
    Ind <- rep(0, dim(H)[2])
    for (i in seq_along(site.ids)) {
      Ind[which(total_site_id == site.ids[i])] <- i
    }
  } else if (by == "pft") {
    pft <- utils::read.csv(pft.path)
    rownames(pft) <- pft$site
    total_site_id <- rep(site.ids, each = length(var.names))
    total_pft <- pft[total_site_id, 2]
    Ind <- rep(0, dim(H)[2])
    pft.names <- sort(unique(pft$pft))
    for (i in seq_along(pft.names)) {
      Ind[which(total_pft == pft.names[i])] <- i
    }
  } else if (by == "block_pft_var") {
    #by pft
    pft <- utils::read.csv(pft.path)
    rownames(pft) <- pft$site
    total_site_id <- rep(site.ids, each = length(var.names))
    total_pft <- pft[total_site_id, 2]
    Ind_pft <- rep(0, dim(H)[2])
    pft.names <- sort(unique(pft$pft))
    for (i in seq_along(pft.names)) {
      Ind_pft[which(total_pft == pft.names[i])] <- i
    }
    #by var
    total_var_name <- rep(var.names, length(site.ids))
    Ind_var <- rep(0, dim(H)[2])
    for (i in seq_along(var.names)) {
      Ind_var[which(total_var_name == var.names[i])] <- i
    }
    #by site
    total_site_id <- rep(site.ids, each = length(var.names))
    Ind_site <- rep(0, dim(H)[2])
    for (i in seq_along(site.ids)) {
      Ind_site[which(total_site_id == site.ids[i])] <- i
    }
    # #create reference to which block and which var
    # #Ind for which site should use which block
    # block.index <- var.index <- Ind_site
    # for (i in seq_along(Ind_site)) {
    #   Ind_block[i] <- Ind_pft[i]
    # }
  } else if (by == "all") {
    Ind <- 1:dim(H)[2]
  } else if (by == "single") {
    Ind <- rep(1, dim(H)[2])
  } else {
    PEcAn.logger::logger.info("Couldn't find the proper by argument!")
    return(0)
  }
  if (by == "block_pft_var") {
    return(list(Ind_pft = Ind_pft[which(apply(H, 2, sum) == 1)],
                Ind_site = Ind_site[which(apply(H, 2, sum) == 1)],
                Ind_var = Ind_var[which(apply(H, 2, sum) == 1)],
                H.ind = which(apply(H, 2, sum) == 1)))
  } else {
    return(list(Q.ind = Ind[which(apply(H, 2, sum) == 1)], 
                H.ind = which(apply(H, 2, sum) == 1),
                H.matrix = H))
  }
  
}