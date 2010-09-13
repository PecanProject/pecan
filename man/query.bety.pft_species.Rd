\name{query.bety.pft_species}
\alias{query.bety.pft_species}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Retrieve list of species associated with the \code{pft}
%%  ~~function to do ... ~~
}
\description{This function queries the BETYdb species and pfts_species
  tables and returns the species.id, genus, and species for all species in the given  \code{pft}.
  This requires that the associations between plant species and plant
  functional type be entered into the database (see .
  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
query.bety.pft_species()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pft} {\code{pft} is a string, but pft must be entered into the
  database. Currently, valid values for \code{pft} include:
  'ebifarm.nfixer, 'ebifarm.c4grass', 'ebifarm.c3grass',
  'ebifarm.forb', 'ebifarm.c4crop', 'poplar'
  }
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Returns a string of ids for each species associated with the \code{pft}.
  The string is used to query traits in the query.bety.traits() function. 
%%  ~Describe the value returned
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
  \code{link{query.bety.traits}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
spp <- query.bety.pft_species('ebifarm.nfixer')
trait.data <- query.bety.traits(
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function().
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
