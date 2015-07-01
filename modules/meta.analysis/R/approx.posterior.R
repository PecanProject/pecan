#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Approximate the posterior MCMC with a closed form pdf
##'
##' returns priors where posterior MCMC are missing
##' 
##' NOTE: this function is similar to PEcAn.priors::fit.dist
##' @title Approximate posterior
##' @param trait.mcmc meta analysis outputs
##' @param priors dataframe of priors used in meta analysis
##' @param trait.data data used in meta-analysis (used for plotting)
##' @param outdir directory in which to plot results
##' @param filename.flag text to be included in the posteriors.pdf filename to make unique
##' @return posteriors data frame, similar to priors,
##' but with closed form pdfs fit to meta-analysis results  
##' @export
##' @author David LeBauer, Carl Davidson, Mike Dietze
##' @examples
##' \dontrun{data("trait.mcmc", package = "PEcAn.utils")
##' data("prior.distns", package = "PEcAn.utils")
##' approx.posterior(trait.mcmc, priors = prior.distns)}
approx.posterior <- function(trait.mcmc, priors, trait.data=NULL, outdir=NULL, filename.flag=""){
  ##initialization
  posteriors <- priors
  do.plot <- exists("outdir")
  if(do.plot == TRUE){
    pdf(file.path(outdir, paste("posteriors", filename.flag, ".pdf", sep="")))
  }
  
  ##loop over traits
  for(trait in names(trait.mcmc)){
    print(trait)
    
    dat    <- trait.mcmc[[trait]]
    vname  <- colnames(dat[[1]])
    dat    <- as.array(dat); if(length(dim(dat))==0) dat <- array(dat,c(length(dat),1,1))
    if(length(dim(dat))>1){
      dat    <- as.vector(as.array(dat)[,which(vname == "beta.o"),])
    }
    pdist  <- priors[trait, "distn"]
    pparm  <- as.numeric(priors[trait, 2:3])
    ptrait <- trait

    fp <- function(x){
      cl <- call(paste("d",priors[ptrait,"distn"],sep=""),x,priors[ptrait,"parama"],
                 priors[ptrait,"paramb"])
      eval(cl)
    }
    
    ## first determine the candidate set of models based on any range restrictions
    zerobound <- c("exp", "gamma", "lnorm", "weibull")
    if(pdist %in% "beta"){
      m <- mean(dat)
      v <- var(dat)
      k <- (1-m)/m
      a <- ( k/((1+k)^2*v)-1)/(1+k)
      b <- a*k
      fit <- try(fitdistr(dat,"beta",list(shape1=a,shape2=b)))      
      if(do.plot){
        x <- seq(0,1,length=1000)
        plot(density(dat), col = 2,lwd = 2,main = trait)
        if(!is.null(trait.data)){
          rug(trait.data[[trait]]$Y, lwd = 2, col="purple") 
        }
        lines(x, dbeta(x, fit$estimate[1], fit$estimate[2]), lwd=2, type='l')
        lines(x, dbeta(x, pparm[1], pparm[2]), lwd = 3, type = 'l', col=3)
        legend("topleft",legend=c("data", "prior", "post", "approx"),
               col = c("purple", 3, 2, 1), lwd = 2)
      }
      posteriors[trait,"parama"] <- fit$estimate[1]
      posteriors[trait,"paramb"] <- fit$estimate[2]
    } else if(pdist %in% zerobound | (pdist == "unif" & pparm[1] > 0)){
      dist.names <- c("exp", "lnorm", "weibull", "norm")
      fit <- list()
      fit[[1]] <- try(suppressWarnings(fitdistr(dat,"exponential")), silent=TRUE)
      ## fit[[2]] <- fitdistr(dat,"f",list(df1=10,df2=2*mean(dat)/(max(mean(dat)-1,1))))
      fit[[2]] <- try(suppressWarnings(fitdistr(dat, "lognormal")), silent=TRUE)
      fit[[3]] <- try(suppressWarnings(fitdistr(dat, "weibull")), silent=TRUE)
      fit[[4]] <- try(suppressWarnings(fitdistr(dat, "normal")), silent=TRUE)
      if(!trait == 'cuticular_cond'){
        fit[[5]] <- try(suppressWarnings(fitdistr(dat, "gamma")), silent=TRUE)
        dist.names <- c(dist.names, "gamma")
      } 
      failfit.bool <- sapply(fit, class) == "try-error"
      fit[failfit.bool] <- NULL
      dist.names <- dist.names[!failfit.bool]
      
      fparm <- lapply(fit,function(x){as.numeric(x$estimate)})
      fAIC  <- lapply(fit,function(x){AIC(x)})
      
      bestfit <- which.min(fAIC)
      posteriors[ptrait,"distn"] <- dist.names[bestfit]
      posteriors[ptrait,"parama"] <- fit[[bestfit]]$estimate[1]
      if(bestfit == 1){
        posteriors[ptrait,"paramb"] <- NA
      } else {
        posteriors[ptrait,"paramb"] <- fit[[bestfit]]$estimate[2]
      }
      
      if(do.plot){
        f <- function(x){
          cl <- call(paste("d",posteriors[ptrait,"distn"],sep=""),x,
                     posteriors[ptrait,"parama"],posteriors[ptrait,"paramb"])
          eval(cl)
        }
        fq <- function(x){
          cl <- call(paste("q",priors[ptrait,"distn"],sep=""),x,
                     priors[ptrait,"parama"],priors[ptrait,"paramb"])
          eval(cl)
        }        
        qbounds <- fq(c(0.01, 0.99))
        x <- seq(qbounds[1], qbounds[2], length = 1000)
        rng <- range(dat)
        if(!is.null(trait.data)) rng = range(trait.data[[trait]]$Y)
        
        plot(density(dat), col=2, lwd=2, main = trait, xlim = rng)
        if(!is.null(trait.data)) {
          rug(trait.data[[trait]]$Y, lwd = 2,col="purple") 
        }
        lines(x, f(x),  lwd=2, type='l')
        lines(x, fp(x), lwd=3, type='l', col=3)
        legend("topleft", legend=c("data", "prior", "post","approx"),
               col = c("purple", 3, 2, 1), lwd = 2)
      }
      
    } else {
    
      ## default: NORMAL
      posteriors[trait,"distn"]  <- "norm"
      posteriors[trait,"parama"] <- mean(dat)
      posteriors[trait,"paramb"] <- sd(dat)
      if(do.plot){
        rng <- quantile(dat, c(0.01, 0.99))
        x <- seq(rng[1], rng[2], length=1000)
        plot(density(dat), col=2, lwd=2, main = trait, xlim = rng)
        if(!is.null(trait.data)) {
          rug(trait.data[[trait]]$Y, lwd = 2, col="purple") 
        }
        lines(x, dnorm(x, mean(dat), sd(dat)), lwd=2, type='l')
        lines(x, fp(x), lwd = 3, type = 'l', col = 3)
        legend("topleft", legend=c("data", "prior", "post", "approx"),
               col = c("purple", 3, 2, 1 ), lwd = 2)
      }
    }
  }  ## end trait loop

  if(do.plot) dev.off()
  
  return(posteriors)
  
}
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.      				
####################################################################################################
