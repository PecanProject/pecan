#-------------------------------------------------------------------------------
# Copyright (c) 2015
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

#' @title allom.predict
#' @name  allom.predict
#' @aliases allom.predict
#' 
#' @param object Allometry model object. Option includes
#'\itemize{
#'   \item{"list of mcmc"}{ - mcmc outputs in a list by PFT then component}
#'   \item{"vector of file paths"}{ - path(s) to AllomAve RData files}
#'   \item{"directory where files are located}{ - }
#' }
#' @param dbh Diameter at Breast Height (cm)
#' @param pft Plant Functional Type. Needs to match the name used in AllomAve. Can be NULL if only one PFT/species exists, otherwise needs to the same length as dbh
#' @param component Which component to predict. Can be NULL if only one component was analysed in AllomAve.
#' @param n Number of Monte Carlo samples. Defaults to the same number as in the MCMC object
#' @param use  c("Bg","mu","best")
#' @param interval c("none","confidence","prediction") default is prediction
#' 
#' @return matrix of Monte Carlo predictions that has n rows and one column per DBH
#'
#' @description Function for making tree-level Monte Carlo predictions from allometric equations estimated from the PEcAn allometry module
#'   
#' 
#' @examples 
#' 
#' \dontrun{
#'   object = "~/Dropbox//HF C Synthesis/Allometry Papers & Analysis/"
#'   dbh = seq(10,50,by=5)
#'   mass = allom.predict(object,dbh,n=100)
#'   
#' }
#' 
#' @author Michael Dietze, Christy Rollinson
#' 
#' @export
#' 
allom.predict <- function(object,dbh,pft=NULL,component=NULL,n=NULL,use="Bg",interval="prediction"){
  library(coda)
  library(tools)
  
  if(class(object) == "character"){
    object = load.allom(object)
  }   

  ## error checking
  npft = length(object)
  if(npft==0){
    print("No PFT objects found")
    return(NA)
  }
  ncomp = max(sapply(object,length))
  if(ncomp <= 1){
    print("No COMPONENTS found")
    return(NA)
  }
  if(is.null(pft)){
    if(npft > 1){
      print("More than 1 Allom PFT specified but 'pft' not provided")
      print(names(object))
    }
  }
  if(any(!(pft %in% names(object)))){
    print("PFT(s) not found in Allom object")
    print(unique(pft[!(pft %in% names(object))]))
    return(NA)
  }
  if(length(pft)==1 & length(dbh)>1){
    pft=rep(pft,length(dbh))
  }
  if(length(pft) != length(dbh)){
    print("ERROR: number of PFT records does not match number of DBH records")
    return(NA)
  }

        
  ## build PFT x Component table and convert mcmclist objects to mcmc
  pftByComp = matrix(NA,npft,ncomp)
  for(i in 1:npft){
    pftByComp[i,] <- !sapply(object[[i]],is.null)
    for(j in which(pftByComp[i,])){
      if(is.mcmc.list(object[[i]][[j]])) object[[i]][[j]] = as.mcmc(as.matrix(object[[i]][[j]]))
    }
  }
  ## set and check component
  if(is.null(component)) component = which.max(apply(pftByComp,2,sum))
#  if(!all(pftByComp[,component])){
  if(!all(unique(pft) %in% names(object)[pftByComp[,component]])){
    print(paste("Missing component",component,"for some PFTs"))
    print(names(object)[!pftByComp[,component]])
    return(NA)
  }
  ## set use
  if(length(use)<npft) use = rep(use,npft)
  ## set n
  if(is.null(n)){
    n = min(sapply(object,function(x){
      y = NA
      for(j in seq_along(x)){
        z = nrow(x[[j]])
        if(!is.null(z)) y[j]=z
      }
      return(min(y,na.rm=TRUE))
    }))
  }
  if(n < 1 | is.na(n)){
    print(paste("invalid n",n))
  }
    
  
  ## Extract relevant parameter vectors
  ## stick in a list by PFT
  params <- list()
  for(i in 1:npft){
    if(length(object[[i]][[component]])==0) next
    if(interval == "none"){
      ## interval = none       -> mean of mu or Bg
      if(use[i] == "Bg"){
        params[[i]] <- apply(object[[i]][[component]][,c("Bg0","Bg1")],2,mean,na.rm=TRUE)  
      } else if(use[i]=="mu"){
        params[[i]] <- apply(object[[i]][[component]][,c("mu0","mu1")],2,mean,na.rm=TRUE) 
      } else {
        print(paste("use =",use[i],"not currently supported"))
        return(NA)
      }            
    } else if(interval == "confidence"){
      ##          = confidence -> sample of (mu/Bg)
      sel = sample.int(nrow(object[[i]][[component]]),n,replace = TRUE)    
      if(use[i] == "Bg"){
        params[[i]] <- object[[i]][[component]][sel,c("Bg0","Bg1")]
      } else if(use[i]=="mu"){
        params[[i]] <- object[[i]][[component]][sel,c("mu0","mu1")] 
        
  #### *** should this case include random effects too ????
        
      } else {
        print(paste("use =",use[i],"not currently supported"))
        return(NA)
      }       
    } else if(interval == "prediction"){
      ##          = prediction -> sample of (mu/Bg), sample of (sigma/Sg), if Bg sample of tau
      sel = sample.int(nrow(object[[i]][[component]]),n,replace = TRUE)    
      if(use[i] == "Bg"){
        params[[i]] <- object[[i]][[component]][sel,c("Bg0","Bg1","Sg")]
      } else if(use[i]=="mu"){
        library(mvtnorm)
        p = object[[i]][[component]][sel,c("mu0","mu1","sigma","tau11","tau12","tau22")] 
        ## pre-sample random effect variability
        mu = matrix(NA,n,2)
        for(j in 1:n){
          tau = matrix(p[j,c("tau11","tau12","tau12","tau22")],2,2)
          mu[j,] = rmvnorm(1,p[j,c("mu0","mu1")],tau)
        }
        params[[i]] <- cbind(mu,p[,"sigma"])
      } else {
        print(paste("use =",use[i],"not currently supported"))
        return(NA)
      }         
    } else {
      print(paste("interval = ",interval,"not supported"))
      return(NA)
    }    
  }
  names(params) = names(object)
  
  ### perform actual allometric calculation
  out = matrix(NA,n,length(dbh))
  for(p in unique(pft)){
    sel = which(pft == p)
    a = params[[p]][,1]
    b = params[[p]][,2]
    if(ncol(params[[p]])>2){
      s = sqrt(params[[p]][,3]) ## sigma was originally calculated as a variance, so convert to std dev
    } else {s = 0}
    for(i in sel){
      out[,i]=exp(rnorm(n,a+b*log(dbh[i]),s))
    }
  }

  
  return(out)
  
}

#' @title load.allom
#' @name  load.allom
#' 
#' @param object Allometry model object. Option includes
#'\itemize{
#'   \item{"vector of file paths"}{ - path(s) to AllomAve RData files}
#'   \item{"directory where files are located}{ - }
#' }
#' 
#' @return mcmc outputs in a list by PFT then component
#'
#' @description loads allom files
#'   
#' @examples 
#' 
#' \dontrun{
#'   object = "~/Dropbox//HF C Synthesis/Allometry Papers & Analysis/"
#'   allom.mcmc = load.allom(object)
#'   
#' }
#' 
#' @author Michael Dietze
#' 
#' @export
#' 
load.allom <- function(object){
  ## assuming object is file path, load up
  tmp <- list()
  for(i in seq_along(object)){
    
    if(tolower(file_ext(object[i])) == "rdata"){
      my.files = object[i]
    } else {
      my.files = dir(object[i],"Allom.*.Rdata",full.names = TRUE)
    }
    
    for(j in seq_along(my.files)){
      ## parse file name  
      my.name = basename(my.files[j])
      my.name.parts = strsplit(my.name,split = ".",fixed=TRUE)[[1]]
      my.pft  = my.name.parts[length(my.name.parts)-2]
      my.comp = as.numeric(my.name.parts[length(my.name.parts)-1])
      
      ## load file itself
      if(my.pft %in% names(tmp)){
        k = which(names(tmp) == my.pft)
      } else {
        k = length(tmp)+1
        tmp[[k]] = list()
        names(tmp)[k] = my.pft
      }
      load(my.files[j])
      tmp[[k]][[my.comp]] = mc
    }
  }
  
  ## convert mcmclist objects to mcmc
  for(i in 1:length(tmp)){
    for(j in which(!sapply(tmp[[i]],is.null))){
      if(is.mcmc.list(tmp[[i]][[j]])) tmp[[i]][[j]] = as.mcmc(as.matrix(tmp[[i]][[j]]))
    }
  }
  
  return(tmp)
  
  
}