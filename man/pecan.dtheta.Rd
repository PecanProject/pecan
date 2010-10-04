\name{pecan.dtheta}
\alias{pecan.dtheta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pecan.dtheta(samps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{samps}{ ~~Describe \code{samps} here~~ }
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

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(samps){
  ##calculate values of:
  ## dtheta_0.35, dtheta_0.5, dtheta_0.65 
  ## var(theta_i)

  traits <- colnames(samps)
  n.traits <- length(traits)

  dtheta <-  matrix(NA, ncol = 5, nrow = n.traits)
  colnames(dtheta) <- c('lcl', 'ucl', 'mean', 'var', 'cv')
  rownames(dtheta) = traits

  dtheta.q <- function(x) {
    if (i.tr != 'Vm_low_temp') {
      c(quantile(x,c(0.35,0.5,0.65)), var(x), sqrt(var(x))/mean(x))
    } else {
      c(quantile(x,c(0.35,0.5,0.65)), var(x), sqrt(var(x))/mean(x+273.15))
    }
  }

  for (i.tr in traits) {
    dtheta.q[i.tr, ] <- dtheta.q(samps[ ,i.tr])
  }
  return(dtheta.q)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
