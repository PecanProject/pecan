# This is based on the mcmc.list2init.IGF function in pecan, but fixes an issue that is probably specific to the inventory growth fusion function:
# For inits that are matrices (i.e. for the state variable x in the inventory growth fusion function), the original function was only filling in the first column of the matrix with the correct init values.
# Lines 57-61 fix this for the inventory growth fusion project, but may not be applicable to other mcmc lists
# additionally, nr is not defined in the orignal function, but I think the "n" should actually be "nr"

mcmc.list2initIGF <- function(dat) {
  library("coda")
  
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
  nr <- nrow(dat[[1]]) # 100 iterations (nrows in the mcmc output)
  nc <- coda::nchain(dat) # 3 chains
  for(c in seq_len(nc)) ic[[c]] <- list() # create ic list 
  
  for(v in seq_along(uname)){ # for each unique variable v
    
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
          ic[[c]][[v]] <- matrix(seq_along(cols), max(ind[cols,1]), max(ind[cols,2])) ## set up matrix for storage--a 644 tree X 50 year
          #ic[[c]][[v]][ind[cols]] <- dat[[c]][nr,cols] # dat [[c]][nr,cols] is a vector with 32200 dbhs --KH fixing this issue
          # need to convert this vector to one where trees are in rows and years are in columns (644 X 50)
          dat.ic <- matrix(dat[[c]][nr, cols], nrow = max(ind[cols,1]), byrow = FALSE)
          ic[[c]][[v]] <- dat.ic
          
          
          
          names(ic[[c]])[v] <- uname[v]
        }
        
      } else {
        PEcAn.utils::logger.severe("dimension not supported",dim,uname[v])
      }
      
    }  ## end else VECTOR or MATRIX
    
  } ## end loop over v
  
  return(ic)
  
} ## end mcmc.list2init
