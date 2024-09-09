
#' allom.predict
#'
#' Function for making tree-level Monte Carlo predictions
#' from allometric equations estimated from the PEcAn allometry module
#'
#' @param object Allometry model object. Option includes
#'\describe{
#'   \item{'list of mcmc'}{ - mcmc outputs in a list by PFT then component}
#'   \item{'vector of file paths'}{ - path(s) to AllomAve RData files}
#'   \item{'directory where files are located}{ - }
#' }
#' @param dbh Diameter at Breast Height (cm)
#' @param pft Plant Functional Type. Needs to match the name used in AllomAve.
#'  Can be NULL if only one PFT/species exists, otherwise needs to the same length as dbh
#' @param component Which component to predict. Can be NULL if only one component was analysed in AllomAve.
#' @param n Number of Monte Carlo samples. Defaults to the same number as in the MCMC object
#' @param use  c('Bg','mu','best')
#' @param interval c('none','confidence','prediction') default is prediction
#' @param single.tree logical: Is this a DBH time series from one individual tree?
#'  If TRUE, will use a fixed error for all draws.
#'
#' @return matrix of Monte Carlo predictions that has n rows and one column per DBH
#'
#'
#' @examples
#'
#' \dontrun{
#'   object = '~/Dropbox//HF C Synthesis/Allometry Papers & Analysis/'
#'   dbh = seq(10,50,by=5)
#'   mass = allom.predict(object,dbh,n=100)
#'
#' }
#'
#' @author Michael Dietze, Christy Rollinson
#'
#' @export
#'
# allom.predict(allom.fit[pft],dbh = dbh,pft = 'BEAL',component = 6,use = 'Bg',interval =
# 'prediction',single.tree=TRUE)
allom.predict <- function(object, dbh, pft = NULL, component = NULL, n = NULL, use = "Bg",
                          interval = "prediction", single.tree = FALSE) {
  
  if (is.character(object)) {
    object <- load.allom(object)
  }
  
  ## error checking
  npft <- length(object)
  if (npft == 0) {
    print("No PFT objects found")
    return(NA)
  }
  ncomp <- max(sapply(object, length))
  if (ncomp <= 1) {
    print("No COMPONENTS found")
    return(NA)
  }
  if (is.null(pft)) {
    if (npft > 1) {
      print("More than 1 Allom PFT specified but 'pft' not provided")
      print(names(object))
    }
  }
  if (any(!(pft %in% names(object)))) {
    print("PFT(s) not found in Allom object")
    print(unique(pft[!(pft %in% names(object))]))
    return(NA)
  }
  if (length(pft) == 1 & length(dbh) > 1) {
    pft <- rep(pft, length(dbh))
  }
  if (length(pft) != length(dbh)) {
    print("ERROR: number of PFT records does not match number of DBH records")
    return(NA)
  }
  
  
  ## build PFT x Component table and convert mcmclist objects to mcmc
  pftByComp <- matrix(NA, npft, ncomp)
  for (i in seq_len(npft)) {
    pftByComp[i, ] <- !sapply(object[[i]], is.null)
    for (j in which(pftByComp[i, ])) {
      if (coda::is.mcmc.list(object[[i]][[j]])) {
        object[[i]][[j]] <- coda::as.mcmc(as.matrix(object[[i]][[j]]))
      }
    }
  }
  ## set and check component
  if (is.null(component)) {
    component <- which.max(apply(pftByComp, 2, sum))
  }
  # if(!all(pftByComp[,component])){
  if (!all(unique(pft) %in% names(object)[pftByComp[, component]])) {
    print(paste("Missing component", component, "for some PFTs"))
    print(names(object)[!pftByComp[, component]])
    return(NA)
  }
  ## set use
  if (length(use) < npft) {
    use <- rep(use, npft)
  }
  ## set n
  if (is.null(n)) {
    n <- min(sapply(object, function(x) {
      y <- NA
      for (j in seq_along(x)) {
        z <- nrow(x[[j]])
        if (!is.null(z)) {
          y[j] <- z
        }
      }
      return(min(y, na.rm = TRUE))
    }))
  }
  if (n < 1 | is.na(n)) {
    print(paste("invalid n", n))
  }
  
  ## Extract relevant parameter vectors stick in a list by PFT
  params <- list()
  for (i in seq_len(npft)) {
    if (length(object[[i]][[component]]) == 0) {
      next
    }
    if (interval == "none") {
      ## interval = none -> mean of mu or Bg
      if (use[i] == "Bg") {
        params[[i]] <- apply(object[[i]][[component]][, c("Bg0", "Bg1")], 2, mean, na.rm = TRUE)
      } else if (use[i] == "mu") {
        params[[i]] <- apply(object[[i]][[component]][, c("mu0", "mu1")], 2, mean, na.rm = TRUE)
      } else {
        print(paste("use =", use[i], "not currently supported"))
        return(NA)
      }
    } else if (interval == "confidence") {
      ## = confidence -> sample of (mu/Bg)
      sel <- sample.int(nrow(object[[i]][[component]]), n, replace = TRUE)
      if (use[i] == "Bg") {
        params[[i]] <- object[[i]][[component]][sel, c("Bg0", "Bg1")]
      } else if (use[i] == "mu") {
        params[[i]] <- object[[i]][[component]][sel, c("mu0", "mu1")]
        
        #### *** should this case include random effects too ????
        
      } else {
        print(paste("use =", use[i], "not currently supported"))
        return(NA)
      }
    } else if (interval == "prediction") {
      ## = prediction -> sample of (mu/Bg), sample of (sigma/Sg), if Bg sample of tau
      sel <- sample.int(nrow(object[[i]][[component]]), n, replace = TRUE)
      if (use[i] == "Bg") {
        params[[i]] <- object[[i]][[component]][sel, c("Bg0", "Bg1", "Sg")]
      } else if (use[i] == "mu") {
        p <- object[[i]][[component]][sel, c("mu0", "mu1", "sigma", "tau11", "tau12", "tau22")]
        ## pre-sample random effect variability
        mu <- matrix(NA, n, 2)
        for (j in seq_len(n)) {
          tau <- matrix(p[j, c("tau11", "tau12", "tau12", "tau22")], 2, 2)
          mu[j, ] <- mvtnorm::rmvnorm(1, p[j, c("mu0", "mu1")], tau)
        }
        params[[i]] <- cbind(mu, p[, "sigma"])
      } else {
        print(paste("use =", use[i], "not currently supported"))
        return(NA)
      }
    } else {
      print(paste("interval = ", interval, "not supported"))
      return(NA)
    }
  }
  names(params) <- names(object)
  
  ### perform actual allometric calculation
  if (methods::is(dbh, "list")) {
    out <- list(length(dbh))
  } else {
    out <- matrix(NA, n, length(dbh))
  }
  for (p in unique(pft)) {
    sel <- which(pft == p)
    a <- params[[p]][,1]
    b <- params[[p]][,2]
    if (ncol(params[[p]]) > 2) {
      s <- sqrt(params[[p]][,3]) ## sigma was originally calculated as a variance, so convert to std dev
    } else {
      s <- 0
    }
    
    if (methods::is(dbh, "list")) {
      for (j in 1:length(sel)) {
        if ((methods::is(dbh[[sel[j]]], "numeric")) & (all(is.na(dbh[[sel[j]]])))) {
          out[[sel[j]]] <- array(NA, c(n,1,length(dbh[[sel[j]]])))
          out[[sel[j]]][,,] <- NA
          next
        } else if (methods::is(dbh[[sel[j]]], "numeric")) {
          ntrees <- 1
          nyears <- length(dbh[[sel[j]]])
        } else {
          ntrees <- nrow(dbh[[sel[j]]])
          nyears <- ncol(dbh[[sel[j]]])
        }
        
        out[[sel[j]]] <- array(NA, c(n,ntrees,nyears))
        
        for (k in 1:ntrees) {
          epsilon <- stats::rnorm(n, 0, s) # don't fix this for a single tree; fix for a single iteration for a single site across all trees
          if (methods::is(dbh[[sel[j]]], "numeric")) {
            dbh_sel_k <- dbh[[sel[j]]]
          } else {
            dbh_sel_k <- dbh[[sel[j]]][k,]
          }
          
          log_x <- sapply(dbh_sel_k, function(x) if(is.na(x)|(x<=0)){return(NA)}else{log(x)})
          out[[sel[j]]][,k,] <-  sapply(log_x, function(x) if(is.na(x)){rep(NA, n)}else{exp(a+b*x + epsilon)})
        }
      }
    } else if (single.tree == TRUE) {
      # for a dbh time-series for a single tree, fix error for each draw
      epsilon = stats::rnorm(n, 0, s)
      for (i in 1:n) {
        out[i,] <- exp(a[i]+b[i]*log(dbh) + epsilon[i])
      }
    } else {
      # for a dbh time-series for different trees, error not fixed across draws
      for (i in sel) {
        out[,i] <- exp(stats::rnorm(n, a+b*log(dbh[i]),s))
      }
    }
    
  }
  
  return(out)
} # allom.predict

#' load.allom
#'
#' loads allom files
#'
#' @param object Allometry model object. Option includes
#'\describe{
#'   \item{'vector of file paths'}{ - path(s) to AllomAve RData files}
#'   \item{'directory where files are located}{ - }
#' }
#'
#' @return mcmc outputs in a list by PFT then component
#'
#' @examples
#'
#' \dontrun{
#'   object = '~/Dropbox//HF C Synthesis/Allometry Papers & Analysis/'
#'   allom.mcmc = load.allom(object)
#'
#' }
#'
#' @author Michael Dietze
#'
#' @export
#'
load.allom <- function(object) {
  ## assuming object is file path, load up
  tmp <- list()
  for (i in seq_along(object)) {
    
    if (tolower(tools::file_ext(object[i])) == "rdata") {
      my.files <- object[i]
    } else {
      my.files <- dir(object[i], "Allom.*.Rdata", full.names = TRUE)
    }
    ## Need to add a 3rd option if the files are remotely on Dropbox
    ## download_file(object,'foo.Rdata',method='curl') works for a single file not sure how to get the
    ## file listing
    
    for (j in seq_along(my.files)) {
      ## parse file name
      my.name <- basename(my.files[j])
      my.name.parts <- strsplit(my.name, split = ".", fixed = TRUE)[[1]]
      my.pft <- my.name.parts[length(my.name.parts) - 2]
      my.comp <- as.numeric(my.name.parts[length(my.name.parts) - 1])
      
      ## load file itself
      if (my.pft %in% names(tmp)) {
        k <- which(names(tmp) == my.pft)
      } else {
        k <- length(tmp) + 1
        tmp[[k]] <- list()
        names(tmp)[k] <- my.pft
      }
      tmp_env <- new.env()
      load(my.files[j], envir = tmp_env)
      tmp[[k]][[my.comp]] <- tmp_env$mc
    }
  }
  
  ## convert mcmclist objects to mcmc
  for (i in seq_along(tmp)) {
    for (j in which(!sapply(tmp[[i]], is.null))) {
      if (coda::is.mcmc.list(tmp[[i]][[j]])) {
        tmp[[i]][[j]] <- coda::as.mcmc(as.matrix(tmp[[i]][[j]]))
      }
    }
  }
  
  return(tmp)
} # load.allom
