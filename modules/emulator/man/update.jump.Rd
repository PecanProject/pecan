\name{update.jump}
\alias{update.jump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
update.jump(jmp, parm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jmp}{ ~~Describe \code{jmp} here~~ }
  \item{parm}{ ~~Describe \code{parm} here~~ }
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
function(jmp,parm){
  ##check for valid typing
  if(is.null(jmp)) stop("jump is NULL")
  
  ##update counter
  cnt <- attr(jmp,"count") + 1;
  attr(jmp,"count") <- cnt;

  ##insert new parm
  attr(jmp,"chain")[cnt,] <- parm

  #print(attr(jmp,"chain"))
  
  ##update jump parm
  if(cnt \%\% attr(jmp,"clen") == 0){
    clen <- attr(jmp,"clen")
    chain <- attr(jmp,"chain")[(cnt-clen):cnt,1]
    a <- arate(chain)
    l <- length(attr(jmp,"history"))
    j <- attr(jmp,"history")[l]
    attr(jmp,"history")[l+1] <- j*a/attr(jmp,"target")
    attr(jmp,"arate")[l+1] <- a
  }
  jmp
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
