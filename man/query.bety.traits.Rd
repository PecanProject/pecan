\name{query.bety.traits}
\alias{query.bety.traits}
\title{Extract trait data from BETYdb for traits with priors   
}
\description{Determines which traits have 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
query.bety.traits()
}
\arguments{
  \item{spstr}{string of species id's in database, can be retrieved for a pft of interestwith
    
    \code{spp <- query.bety.pft_species(pft)$spstr} generated}
  \item{trvec}{vector of traits to query, can be retrieved based on the existance of priors for a given pft with

  \code{rownames(query.bety.priors(pft))}}
\details{
This function applies \code{query.bety.trait.data()} to values in
\code{trvec} for which data exist for given species in BETYdb
}
\value{
  A list of data sets for each trait, which are ready for use by the
  meta-analysis in \code{pecan.ma}. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{David~S. LeBauer
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{query.bety.traits.data}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
spstr <- query.bety.pft_species('ebifarm.c4crop')$spstr
trvec <- c('leafN', 'SLA')
trait.data <- query.bety.traits(spstr, trvec)
}
 
\keyword{IO}
\keyword{database}
