##' @title GrabFillMatrix
##' @name  GrabFillMatrix
##' @author Dongchen Zhang
##' 
##' @param M  source matrix that will be either subtracted or filled in.
##' @param ind vector of index that of where to be subtracted or filled in.
##' @param M1 additional matrix used to fill in the source matrix, the default it NULL.
##' @details This function helps subtract or fill in a matrix given the index.
##' 
##' @export
GrabFillMatrix <- function (M, ind, M1 = NULL) {
  if (is.null(M1)) {
    #grab a sub-matrix
    m <- matrix(NA, length(ind), length(ind))
    for (i in seq_along(ind)) {
      for (j in seq_along(ind)) {
        m[i, j] <- M[ind[i], ind[j]]
      }
    }
  } else {
    #fill into a larger matrix
    m <- M
    for (i in seq_along(ind)) {
      for (j in seq_along(ind)) {
        m[ind[i], ind[j]] <- M1[i, j]
      }
    }
  }
  m
}

##' @title matrix_network
##' @name  matrix_network
##' @author Dongchen Zhang
##' 
##' @param mat a boolean matrix representing the interactions between any sites.
##' 
##' @return It returns lists of index representing each network.
##'
##' @export
matrix_network <- function (mat) {
  #initialize the final returned list.
  vec_group <- vector("list", ncol(mat))
  #initialize the vector for sites that are completed.
  sites.complete <- c()
  for (i in 1:ncol(mat)) {
    #if we already completed the ith site, go next.
    if (i %in% sites.complete) {
      next
    }
    #initialize the arguments for the while loop.
    vec <- c()
    stop <- FALSE
    inits <- i
    #while loop
    while (!stop) {
      Inits <- c()
      for (init in inits) {
        Inits <- c(Inits, which(mat[init,]))
      }
      Inits <- Inits[which(!Inits %in% vec)]
      vec <- sort(unique(c(vec, Inits)))
      #if we don't have any new site that belongs to this network.
      if (length(Inits) == 0) {
        #then stop.
        stop <- !stop
      } else {
        #else we initialize a new round of searching by new sites.
        inits <- Inits
      }
    }
    sites.complete <- c(sites.complete, vec)
    vec_group[[i]] <- sort(vec)
  }
  vec_group[sapply(vec_group, is.null)] <- NULL
  return(vec_group)
}