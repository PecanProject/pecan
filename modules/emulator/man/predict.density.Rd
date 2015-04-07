\name{predict.density}
\alias{predict.density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.density(den, xnew)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{den}{ ~~Describe \code{den} here~~ }
  \item{xnew}{ ~~Describe \code{xnew} here~~ }
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
function(den,xnew){
##function does simple interpolation of a density object to new points
  neval <- length(den$x)
  nnew <- length(xnew)
  ynew <- rep(NA,nnew)
  for(i in 1:nnew){
    j <- findInterval(xnew[i],den$x)
    if(j == 0 || j==neval){
      ynew[i] <- 0 ## don't extrapolate beyond range,set to 0
    }else {
      ynew[i] <- den$y[j] + (den$y[j+1]-den$y[j])/(den$x[j+1]-den$x[j])*(xnew[i]-den$x[j])
    }   
  }
  ynew
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
