#' Convert mcmc.list to initial condition list
#'
#' Used for restarting MCMC code based on last parameters sampled (e.g. in JAGS)
#'
#' @author Mike Dietze
#'
#' @param dat mcmc.list object
#'
#' @return list
#' @export
#'
mcmc.list2init <- function(dat) {
  need_packages("coda")
  
  ## get unique variable names
  allname <- strsplit(colnames(dat[[1]]),"[",fixed = TRUE)
  firstname <- sapply(allname,function(x){x[1]})
  dims <- lapply(allname,function(x){
    y <- sub(pattern = "]",replacement = "",x[2])
    y <- as.numeric(strsplit(y,",",fixed=TRUE)[[1]])
    return(y)
  })
  ind <- t(sapply(dims,function(x){
    if(length(x)==2){
      return(x)
    } else { return(c(NA,NA))}
  }))
  
  uname <- unique(firstname)
  
  ## define variables
  ic <- list()
  nr <- nrow(dat[[1]])
  nc <- coda::nchain(dat)
  for(c in seq_len(nc)) ic[[c]] <- list()
  
  for(v in seq_along(uname)){
    
    ## detect variable type (scalar, vector, matrix)
    cols <- which(firstname == uname[v])
    if(length(cols) == 1){
      ## SCALAR
      for(c in seq_len(nc)){
        ic[[c]][[v]] <- dat[[c]][nr,cols]
        names(ic[[c]])[v] <- uname[v]
      }
      
    } else {
      
      dim <- length(dims[[cols[1]]])
      
      if(dim == 1){
        ## VECTOR
        for(c in seq_len(nc)){
          ic[[c]][[v]] <- dat[[c]][nr,cols]
          names(ic[[c]])[v] <- uname[v]
        }
        
      } else if (dim == 2){
        ## MATRIX
        for(c in seq_len(nc)){
          ic[[c]][[v]] <- matrix(seq_along(cols),max(ind[cols,1]),max(ind[cols,2])) ## set up matrix for storage
          ic[[c]][[v]][ind[cols]] <- dat[[c]][nr,cols]
          names(ic[[c]])[v] <- uname[v]
        }
        
      } else {
        PEcAn.logger::logger.severe("dimension not supported",dim,uname[v])
      }
      
    }  ## end else VECTOR or MATRIX
    
  } ## end loop over v
  
  return(ic)
  
} ## end mcmc.list2init
