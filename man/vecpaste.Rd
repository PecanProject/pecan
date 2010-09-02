\name{vecpaste}
\alias{vecpaste}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for turning a vector into a comma delimited string
%%  ~~function to do ... ~~
}
\description{
This function takes a vector of strings and turns them into a comma
delimited string. This can be used to make strings suitable for the
paste function from vectors of strings.

In PECAn, this function is used, in conjunction with the paste function, to construct MySQL queries.  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vecpaste(x)
}
}
\arguments{
  \item{x}{
    x is a vector of strings
    %%     ~~Describe \code{model} here~~
}
%- maybe also 'usage' for other objects documented here.
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  The vecpaste function returns a comma delimited string of values from
  the vector, x.
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
  query.bety.traits, query.bety.priors, query.prarie.pft_species, check.bety.data
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

##A simple example
traits <- c('leafN','leaf_width','SLA')
trstr  <- vecpaste(traits)
trstr

##Use in a MySQL query, requires connection to BETY-db database (http:/ebi-forecast.igb.uiuc.edu)
qd   <- paste("SELECT * FROM TraitData WHERE TraitVarID in(", trstr,");", sep = "")
q    <- dbSendQuery(con, qd)


## The function is currently defined as
function(x){
  y <- paste("'",x[1],sep="")
  if(length(x) > 1){
    for(i in 2:length(x)){history 
                          y <- paste(y,x[i],sep="','")
                        }
  }
  y <- paste(y,"'",sep="")
  y
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
