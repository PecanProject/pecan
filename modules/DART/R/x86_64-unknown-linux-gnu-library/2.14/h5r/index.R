##
## This file is a test/debug file for the correct implementation of
## the '[' function. Mostly, it is a file to better understand the
## semantics of argument handling with this function.
##
require(methods)

VERBOSE <- TRUE

setClass('myExample', representation = representation(x = "array"))
setClassUnion('index', members = c("numeric", "character", "logical"))

setGeneric('toNumeric', function(x) standardGeneric('toNumeric'))
setMethod('toNumeric', 'numeric', function(x) x)
setMethod('toNumeric', 'logical', function(x) which(x))


myExample <- function(dims = c(1,2)) {
  a <- array(rnorm(prod(dims)))
  dim(a) <- dims
  obj <- new("myExample")
  obj@x <- a
  return(obj)
}

setMethod("dim", "myExample", function(x) if (length(dim(x@x))) NULL else dim(x@x))

printCall <- function(cl, f, i) {
  cat("call:     ", toString(cl), "\n")
  cat("arguments:", paste(names(f[1:i]), collapse = ", "), "\n\n")
}

setMethod("[", c("myExample", "index", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
  if (VERBOSE) printCall(match.call(), formals(), nargs())
  if (is.null(dim(x))) {
    if (nargs() > 2)
      stop("incorrect number of dimensions")
    x@x[toNumeric(i)]
  } else {
    callGeneric(x, i, j = seq.int(1, dim(x)[2]), ..., drop = drop)
  }
})

setMethod("[", c("myExample", "missing", "missing", "ANY"), function(x, i, j, ..., drop = TRUE) {
  if (VERBOSE) printCall(match.call(), formals(), nargs())
  if (nargs() >= 4) {
    callGeneric(x, i = seq.int(1, dim(x)[1]), j = seq.int(1, dim(x)[2]), ..., drop = drop)
  } else {
    x@x[]
  }
})

setMethod("[", c("myExample", "index", "index", "ANY"), function(x, i, j, ..., drop = TRUE) {
  if (VERBOSE) printCall(match.call(), formals(), nargs())

  print(missing(...))
  print(as.list(match.call()))
  browser()
  match.call()[4:nargs()+1]
  
  x@x[toNumeric(i), toNumeric(j), ..., drop = drop]
})

m1 <- myExample(10)
m2 <- myExample(c(10,10))
m3 <- myExample(c(10,10,10))
m4 <- myExample(c(10,10,10,10))

m1[]
m1[1:3]
m1[1,]
m1@x[1,]
m1[-1]

m4[1:2, 1:2, 1:2, 1:2] == m4@x[1:2, 1:2, 1:2, 1:2]
m4@x[1:2, 1:2, , ]
m4@x[1:2, , , ]

## ############### 2-D
m <- myExample(c(10, 10))
m@x[c(1,5), c(1,5)] == m[c(1,5), c(1,5)]
m@x[c(5, 2),] == m[c(5,2),]

## ############### 3-D
m <- myExample(c(1,3,4))










## functionThatCanOnlyGrabContiguous <- function(x, m, kall) {
##   kall$x <- x@x
##   for (i in 1:nrow(m)) {
##     kall[[i+2]] <- seq.int(m[i,1], m[i,2])
##   }
##   print(as.list(kall))
##   return(eval(kall))
## }

## setMethod("[", "myExample", function(x, i, j, ..., drop = TRUE) {
##   if (nargs() > 

##   m <- matrix(nrow = length(dim(x)), ncol = 2)
  
##   if (missing(i))
##     m[1,] <- c(1, dim(x)[1])
##   else
##     m[1,] <- range(i)

##   if (length(dim(x)) > 1) {
##     if (missing(j))
##       m[2,] <- c(1, dim(x)[2])
##     else
##       m[2,] <- range(j)

##     k <- 3
##     while (k <= nrow(m)) {
##       if (k-2 <= length(e))
##         m[k,] <- range(e[[k-2]])
##       else
##         m[k,] <- c(1, dim(x)[k])
##       k <- k + 1
##     }
##   }
##   kall <- match.call()
##   d <- functionThatCanOnlyGrabContiguous(x, m, kall)

##   kall$x <- d
##   if (! missing(i)) {
##     kall[[3]] <- i - min(i) + 1
##   }
##   if (! missing(j)) {
##     kall[[4]] <- j - min(j) + 1
##   } else {
##     if (length(dim(x)) > 1) 
##       kall[[4]] <- seq.int(1, dim(x)[2])
##   }
##   ## XXX: Have to handle remaining dimensions, but since I can't
##   ## really get a clean '...' it is on hold.
  
##   eval(kall)
## })

## ## ############### 1-D
## m <- myExample(10)
## m@x[c(1,5)] == m[c(1, 5)]

## ## ############### 2-D
## m <- myExample(c(10, 10))
## m@x[c(1,5), c(1,5)] == m[c(1,5), c(1,5)]
## m@x[c(5, 2),] == m[c(5,2),]

## ## ############### 3-D
## m <- myExample(c(1,3,4))

## ## (A) doesn't work

##           m@x[1,1:2,] == m[1,1:2,]

## ## (B) nor does this for different reasons.
## m[1,,1]
## m@x[1,,1]




      dMissing <- TRUE
      if (! missing(drop)) {
        dMissing <- FALSE
      }
            ## All the complexity occurs
      
  ###     nArgs <- nargs()
###       kall  <- as.list(match.call())
###       kall  <- if (! dMissing) kall[-length(kall)]
###       nArgs <- if (! dMissing) nArgs - 1 else nArgs
      
      
###       if (nArgs > 3) {
###         start <- 3
###         start <- if (! iMissing) start + 1 else start
###         start <- if (! jMissing) start + 1 else start
        
###         extras <- kall[start:nArgs]
###         browser()
###                                         # extras <- Filter(function(x) class(x) == "call" || class(x) == "numeric", extras)
###         extras <- lapply(extras, eval)
###       } else {
###         extras <- list()
###       }
      
