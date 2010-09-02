\name{query.bety.priors}
\alias{query.bety.priors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Query priors for a given plant functional type
%%  ~~function to do ... ~~
}
\description{This function queries the priors distributions associated
  with a plant functional type in the BETYdb database. These priors are
  used either in the meta-analysis (\code{traitMA()}) or are sampled
  from directly when pft level data are not available in the database. The required
  input is the string, \code{pft}. The output is a dataframe named
  'priors' with the trait name, the distribution, the parameters, and
  the number of data used to calculate the prior distribution.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
query.bety.priors(pft, prstr)
}
\arguments{
  \item{pft}{
    ~~Describe \code{pft} here~~
  }
  \item{prstr}{
    ~~Describe \code{prstr} here~~
  }
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a dataframe with the prior distributions for each trait.
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
function(pft, prstr){
## prior traits will be used in meta-analysis for traits w/ data
## or to query priors for traits w/o data
  dvr <- dbDriver ("MySQL")
  con <- dbConnect(dvr, group  = "biofuel" )
  query <- paste("SELECT Priors.PriorID, PriorPFT, VarID, PriorDistn, PriorParamA, PriorParamB, PriorN from Priors, Priors_has_PFT where Priors.PriorID = Priors_has_PFT.PriorID and Priors_has_PFT.pftID in ('", pft, "')", sep = "")
  q    <- dbSendQuery(con, query)
  priors <- NA
  priors <- fetch ( q, n = -1 )
  rownames(priors) <- priors$VarID
  return(priors)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
