\name{allom.components}
\alias{allom.components}
\docType{data}
\title{allom.components}
\description{
Allometry components table from Jenkins et al 2004, augmented with additional variables (IDs >= 40
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(allom.components)}
\format{
  A data frame with 43 observations on the following 3 variables.
  \describe{
    \item{\code{description}}{string}
    \item{\code{abbreviation}}{character, See BIOPAK compilation in Means et al. (1994).}
    \item{\code{ID}}{numeric ID}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Jenkins JC, Chojnacky DC, Heath LS, Birdsey RA. 2004. Comprehensive database of diameter-based biomass regressions for north american tree species.  US Forest Service General Technical Report NE-319
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(allom.components)
## maybe str(allom.components) ; plot(allom.components) ...
}
\keyword{datasets}
