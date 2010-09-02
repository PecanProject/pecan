\name{query.bety.parameters}
\alias{query.bety.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
query.bety.parameters(pft, edtraits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pft}{ ~~Describe \code{pft} here~~ }
  \item{edtraits}{ ~~Describe \code{edtraits} here~~ }
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
function(pft, edtraits){
  dvr <- dbDriver ("MySQL")
  con <- dbConnect(dvr, group  = "biofuel" )
  query <- paste("select VarID from Priors where Priors.PriorID in (select Priors_has_PFT.PriorID from Priors_has_PFT where Priors_has_PFT.PFTID = '",pft,"' and VarID in (", vecpaste(edtraits),"));", sep = '')
  #query <- paste('select distinct variable_id from traits where traits.specie_id in (select specie_id from pfts_species where pft_id = (select pfts.id from pfts where pfts.name = ',pft,'));')
  query.result <- dbSendQuery(con, query)
  parms <- fetch(query.result, n = -1)[,1]
  return(parms)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
