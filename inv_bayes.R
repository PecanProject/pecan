#'@title Prospect Bayesian Inversion
#'@author Alexey Shiklomanov

source("prospect.R")
source("timer.R")
source("truncnorm.R")

samp.inits <- list(N=1, 
                   Cab=30,
                   Cw=0.01,
                   Cm=0.005,
                   pwl=rep(0.01, 2101))

## NOTE: obs.spec must be a matrix as follows:
## Column 1 : Wavelengths (400:2500)
## Columns 2-n : Reflectance observations
## Use specdatproc script to generate correct matrices from data.
pinvbayes <- function(obs.spec, prospect=prospect4, ngibbs=100,
                      initc=samp.inits,
                      JumpRSD=5e-4,
                      local.store=FALSE,
                      sample.together=FALSE, 
                      fname = sprintf("runs/%s_%g.dat", deparse(substitute(obs.spec)), JumpRSD))
  {
  wl <- min(obs.spec[,1]):max(obs.spec[,1])
  nwl <- length(wl)
  nspec <- ncol(obs.spec)
  JumpSD <- JumpRSD * unlist(initc)[-5]
  
  ### Initial values unpacked
  N.i <- initc[["N"]]
  Cab.i <- initc[["Cab"]]
  Cw.i <- initc[["Cw"]]
  Cm.i <- initc[["Cm"]]
  pwl.i <- initc[["pwl"]]
  
  
  ### Priors
  N.s <- c(0, 1.5)                # Halfnormal (N = 1 + rlnorm)
  
  # Based on histograms in Feret et al. 2008
  Cab.s <- c(log(30), 0.9)          # Lognormal
  Cw.s <- c(log(0.017), 0.5)        # Lognormal
  Cm.s <- c(log(0.006), 0.9)        # Lognormal
  
  pwl.s <- c(0.001, 0.001)          # Inverse gamma
  
  # Precalculate first model and posterior
  prev.spec <- prospect(N.i, Cab.i, Cw.i, Cm.i)
  prev.error <- -apply(obs.spec[,-1], 2, "-", prev.spec[,"Reflectance"])
  
  pp1 <- sum(dnorm(prev.error, 0, 1/sqrt(pwl.i), log=TRUE))  # Likelihood
  pp2 <- dnorm(N.i - 1, N.s[1], N.s[2], log=TRUE) + log(2)    # N prior
  pp3 <- dlnorm(Cab.i, Cab.s[1], Cab.s[2], log=TRUE)    # Cab prior
  pp4 <- dlnorm(Cw.i, Cw.s[1], Cw.s[2], log=TRUE)     # Cw prior
  pp5 <- dlnorm(Cm.i, Cm.s[1], Cm.s[2], log=TRUE)     # Cm prior
  prev.posterior <- pp1 + pp2 + pp3 + pp4 + pp5
  
  ### MCMC storage
  if (local.store){
          N.store <- numeric(ngibbs)
          Cab.store <- numeric(ngibbs)
          Cw.store <- numeric(ngibbs)
          Cm.store <- numeric(ngibbs)
          pwl.store <- matrix(NA, nrow=ngibbs, ncol=nwl)
  } else {
          pvec <- paste("p", wl, sep='')
          write(c("an", "N", "Cab", "Cw", "Cm", pvec),
                ncolumns=5+nwl,
                file=fname, 
                sep=",")
  }
  ### Define sampler functions
  try.error <- function(N, Cab, Cw, Cm){

          ## Calculate modeled spectra and residuals
          guess.spec <- prospect(N, Cab, Cw, Cm)
          guess.error <- -apply(obs.spec[,-1], 2, "-",
                                guess.spec[,"Reflectance"])
          return(guess.error)
  }

  try.posterior <- function(N, Cab, Cw, Cm, guess.error){

          ## Evaluate posterior | PROSPECT
          gp1 <- sum(dnorm(guess.error, 0, 1/sqrt(pwl.i), log=TRUE))  # Likelihood
          gp2 <- dlnorm(N - 1, N.s[1], N.s[2], log=TRUE) + log(2)    # N prior
          gp3 <- dlnorm(Cab, Cab.s[1], Cab.s[2], log=TRUE)    # Cab prior
          gp4 <- dlnorm(Cw, Cw.s[1], Cw.s[2], log=TRUE)     # Cw prior
          gp5 <- dlnorm(Cm, Cm.s[1], Cm.s[2], log=TRUE)     # Cm prior
          guess.posterior <- gp1 + gp2 + gp3 + gp4 + gp5
          return(guess.posterior)
  }

  sample.all <- function(N.i, Cab.i, Cw.i, Cm.i, JumpSD){

          A <- 0
          ## Draw PROSPECT parameters
          guess.N <- rtnorm(1, N.i, JumpSD[1], Min=1)
          guess.Cab <- rtnorm(1, Cab.i, JumpSD[2])
          guess.Cw <- rtnorm(1, Cw.i, JumpSD[3])
          guess.Cm <- rtnorm(1, Cm.i, JumpSD[4])

          guess.error <- try.error(guess.N, guess.Cab, guess.Cw, guess.Cm)
          guess.posterior <- try.posterior(guess.N, guess.Cab, guess.Cw, guess.Cm, guess.error)

          ## Test acceptance w/ Jump Distribution
          jn1 <- dtnorm(guess.N, N.i, JumpSD[1], Min=1)
          jn2 <- dtnorm(guess.Cab, Cab.i, JumpSD[2])
          jn3 <- dtnorm(guess.Cw, Cw.i, JumpSD[3])
          jn4 <- dtnorm(guess.Cm, Cm.i, JumpSD[4])
          jnum <-jn1 + jn2 + jn3 + jn4

          jd1 <- dtnorm(N.i, guess.N, JumpSD[1], Min=1)
          jd2 <- dtnorm(Cab.i, guess.Cab, JumpSD[2])
          jd3 <- dtnorm(Cw.i, guess.Cw, JumpSD[3])
          jd4 <- dtnorm(Cm.i, guess.Cm, JumpSD[4])
          jden <- jd1 + jd2 + jd3 + jd4

          a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

          if(is.na(a)) a <- -1
          if(a > runif(1)){
                  N.i <- guess.N
                  Cab.i <- guess.Cab
                  Cw.i <- guess.Cw
                  Cm.i <- guess.Cm
                  prev.error <- guess.error
                  prev.posterior <- guess.posterior
                  A <- 1 
          }
          return(list(N.i, Cab.i, Cw.i, Cm.i, prev.posterior, A, prev.error))
  }

  sample.N <- function(N.i, Cab.i, Cw.i, Cm.i, JumpSD){

          A <- 0
          ## Draw PROSPECT parameters
          guess.N <- rtnorm(1, N.i, JumpSD, Min=1)
          guess.error <- try.error(guess.N, Cab.i, Cw.i, Cm.i)
          guess.posterior <- try.posterior(guess.N, Cab.i, Cw.i, Cm.i, guess.error)

          ## Test acceptance w/ Jump Distribution
          jnum <- dtnorm(guess.N, N.i, JumpSD, Min=1)
          jden <- dtnorm(N.i, guess.N, JumpSD, Min=1)
          a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

          if(is.na(a)) a <- -1
          if(a > runif(1)){
                  N.i <- guess.N
                  prev.error <- guess.error
                  prev.posterior <- guess.posterior
                  A <- 1
          }
          return(list(N.i, prev.posterior, A, prev.error))
  }

  sample.Cab <- function(N.i, Cab.i, Cw.i, Cm.i, JumpSD){

          A <- 0
          ## Draw PROSPECT parameters
          guess.Cab <- rtnorm(1, Cab.i, JumpSD)
          guess.error <- try.error(N.i, guess.Cab, Cw.i, Cm.i)
          guess.posterior <- try.posterior(N.i, guess.Cab, Cw.i, Cm.i, guess.error)

          ## Test acceptance w/ Jump Distribution
          jnum <- dtnorm(guess.Cab, Cab.i, JumpSD)
          jden <- dtnorm(Cab.i, guess.Cab, JumpSD)
          a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

          if(is.na(a)) a <- -1
          if(a > runif(1)){
                  Cab.i <- guess.Cab
                  prev.error <- guess.error
                  prev.posterior <- guess.posterior
                  A <- 1
          }
          return(list(Cab.i, prev.posterior, A, prev.error))
  }

  sample.Cw <- function(N.i, Cab.i, Cw.i, Cm.i, JumpSD){

          A <- 0
          ## Draw PROSPECT parameters
          guess.Cw <- rtnorm(1, Cw.i, JumpSD)
          guess.error <- try.error(N.i, Cab.i, guess.Cw, Cm.i)
          guess.posterior <- try.posterior(N.i, Cab.i, guess.Cw, Cm.i, guess.error)

          ## Test acceptance w/ Jump Distribution
          jnum <- dtnorm(guess.Cw, Cw.i, JumpSD)
          jden <- dtnorm(Cw.i, guess.Cw, JumpSD)
          a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

          if(is.na(a)) a <- -1
          if(a > runif(1)){
                  Cw.i <- guess.Cw
                  prev.error <- guess.error
                  prev.posterior <- guess.posterior
                  A <- 1
          }
          return(list(Cw.i, prev.posterior, A, prev.error))
  }

  sample.Cm <- function(N.i, Cab.i, Cw.i, Cm.i, JumpSD){

          A <- 0
          ## Draw PROSPECT parameters
          guess.Cm <- rtnorm(1, Cm.i, JumpSD)
          guess.error <- try.error(N.i, Cab.i, Cw.i, guess.Cm)
          guess.posterior <- try.posterior(N.i, Cab.i, Cw.i, guess.Cm, guess.error)

          ## Test acceptance w/ Jump Distribution
          jnum <- dtnorm(guess.Cm, Cm.i, JumpSD)
          jden <- dtnorm(Cm.i, guess.Cm, JumpSD)
          a <- exp((guess.posterior - jnum) - (prev.posterior - jden))

          if(is.na(a)) a <- -1
          if(a > runif(1)){
                  Cm.i <- guess.Cm
                  prev.error <- guess.error
                  prev.posterior <- guess.posterior
                  A <- 1
          }
          return(list(Cm.i, prev.posterior, A, prev.error))
  }

  
  ## MCMC loop
  tstart <- proc.time()
  ar <- 0
  for(g in 1:ngibbs){
    if((g == 5) | (g %% (ngibbs/20) == 0)){
      laptime(tstart, g, ngibbs)
      cat(sprintf("  AN: %d, AR: %.1f percent      ",
                  ar, (ar/(4 - 3*sample.together))/g * 100))
    }  
  
  if(sample.together){
          prospect.i <- sample.all(N.i, Cab.i, Cw.i, Cm.i, JumpSD)
          N.i <- prospect.i[[1]]
          Cab.i <- prospect.i[[2]]
          Cw.i <- prospect.i[[3]]
          Cm.i <- prospect.i[[4]]
          prev.posterior <- prospect.i[[5]]
          prev.error <- prospect.i[[7]]
          ar <- ar + prospect.i[[6]]
  } else {
          N.sample <- sample.N(N.i, Cab.i, Cw.i, Cm.i, JumpSD[1])
          N.i <- N.sample[[1]]
          prev.posterior <- N.sample[[2]]
          prev.error <- N.sample[[4]]
          ar <- ar + N.sample[[3]]

          Cab.sample <- sample.Cab(N.i, Cab.i, Cw.i, Cm.i, JumpSD[2])
          Cab.i <- Cab.sample[[1]]
          prev.posterior <- Cab.sample[[2]]
          prev.error <- Cab.sample[[4]]
          ar <- ar + Cab.sample[[3]]

          Cw.sample <- sample.Cw(N.i, Cab.i, Cw.i, Cm.i, JumpSD[3])
          Cw.i <- Cw.sample[[1]]
          prev.posterior <- Cw.sample[[2]]
          prev.error <- Cw.sample[[4]]
          ar <- ar + Cw.sample[[3]]

          Cm.sample <- sample.Cm(N.i, Cab.i, Cw.i, Cm.i, JumpSD[4])
          Cm.i <- Cm.sample[[1]]
          prev.posterior <- Cm.sample[[2]]
          prev.error <- Cm.sample[[4]]
          ar <- ar + Cm.sample[[3]]
  }

    ### Sample error precision ### 
    u1 <- pwl.s[1] + nspec/2
    u2 <- pwl.s[2] + 0.5 * apply(prev.error^2, 1, sum)
    pwl.i <- rgamma(nwl, u1, u2)
     
    # Store values 
    if (local.store){
            N.store[g] <- N.i
            Cab.store[g] <- Cab.i
            Cw.store[g] <- Cw.i
            Cm.store[g] <- Cm.i
            pwl.store[g,] <- pwl.i  
    } else {
            write(c(ar, N.i, Cab.i, Cw.i, Cm.i, pwl.i), 
                  ncolumns=5+nwl,
                  sep=",",
                  file=fname,
                  append=TRUE)
    }
  }

  if (local.store){
          return(list(N=N.store, Cab=Cab.store, Cw=Cw.store, Cm=Cm.store, pwl=pwl.store, arate=ar/ngibbs))
  }
}

