\name{lhc}
\alias{lhc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lhc(x, n.samp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{n.samp}{ ~~Describe \code{n.samp} here~~ }
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
function(x,n.samp){
  ## Latin Hyper Cube
  ## simple uniform sampling w/ LHC permutation
  ## x <- list (n.dim x 2)
  ## n.samp <- number of samples
  n.dim <- nrow(x)
  samp <- permute <- matrix(runif(n.dim*n.samp),n.dim,n.samp)
  for(i in 1:n.dim) permute[i,] <- order(permute[i,])
  for(i in 1:n.dim) {
    myseq <- seq(x[i,1],x[i,2],length=n.samp+1)
    samp[i,] <- runif(n.samp,myseq[permute[i,]],myseq[permute[i,]+1])
  }
  t(samp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
