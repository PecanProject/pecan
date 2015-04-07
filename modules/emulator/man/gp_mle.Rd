\name{gp_mle}
\alias{gp_mle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gp_mle(theta, d, nugget)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{ ~~Describe \code{theta} here~~ }
  \item{d}{ ~~Describe \code{d} here~~ }
  \item{nugget}{ ~~Describe \code{nugget} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(theta,d,nugget){

  ##get parms
  mu <- theta[1]
  tauw <- theta[2]; if(tauw<= 0) return(Inf)
  i <- 3; tauv <- 0; if(tauv < 0) return(Inf)
  if(nugget){ tauv <- theta[3]; i <- 4}
  phi <- 1;
  for(k in i:length(theta)){phi[k-i+1] <- theta[k]}
  if(min(phi)<= 0) return(Inf)
  n <- length(y);
  
  S <- calcSpatialCov(d,phi,tauw)

  return(-sum(dmvnorm(y,rep(mu,n),S + diag(tauv,n),log=TRUE)))
  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
