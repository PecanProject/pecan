\name{ma.model}
\alias{ma.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The generic meta-analysis model used with PECAn
%%  ~~function to do ... ~~ 
}
\description{
   The function ma.model is not functional and has no arguments. It is
   only a template model for the write.ma.model function. Its use is analagous to the use of model with the
write.model function in the R2WinBUGS package
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ma.model()
# not used directly. Intended for use with the write.ma.model function.
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(){
  for (k in 1:LENGTHK){
    Y[k] ~ dnorm( Z[k] , tau.y[k])              # observed site x trt means and uncertainties
    Z[k] <- b.trt[trt[k]] + theta[site[k]]      # linear model with random treatment and site effects
    tau.y[k] <- prec.y*n[k]                     # precision from obs.prec
    u1[k] <- n[k]/2                             
    u2[k] <- n[k]/(2*prec.y)
    obs.prec[k] ~ dgamma(u1[k], u2[k])          # observed precision
  }
  b.trt[1] <- 0                                 # control treatment effect = 0
  for ( j in 2:LENGTHJ){
    b.trt[j] ~ dnorm(0, tau.trt)                # treatment effects, random
  }
  for (g in 1:LENGTHG){
    theta[g] ~ dnorm(beta.o, tau.theta)         # site effects, random
  }
  beta.o    ~ PRIORDIST (PRIORPARAMA, PRIORPARAMB) # informed prior on global mean
  tau.theta ~ dgamma(0.1, 0.1)                  # global precision
  tau.trt   ~ dgamma(0.1, 0.1)                  # treatment effect precision
  prec.y    ~ dgamma(0.1, 0.1)                  # observation Y precision
  thetaSD  <- 1 / sqrt(tau.theta)               # across site variance
  ySD      <- 1 / sqrt(prec.y)                  # within site variance
  trtSD    <- 1 / sqrt(tau.trt)                 # within trt variance
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
