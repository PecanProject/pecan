\name{plot.jump}
\alias{plot.jump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.jump(jmp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{jmp}{ ~~Describe \code{jmp} here~~ }
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
function(jmp){
  par(mfrow=c(1,2))
  plot(attr(jmp,"history"),ylab="Jump Parameter",main="Jump Parameter")
  abline(h=mean(attr(jmp,"history"),na.rm=TRUE))
  plot(attr(jmp,"arate"),main="Acceptance Rate",ylim=c(0,1),ylab="Acceptance Rate")
  abline(h=mean(attr(jmp,"arate"),na.rm=TRUE))
  abline(h=attr(jmp,"target"),col=2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
