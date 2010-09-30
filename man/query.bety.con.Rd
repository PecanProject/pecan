\name{query.bety.con}
\alias{query.bety.con}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Setup connection to BETYdb}
\description{
This is a generic function that sets a connection to BETYdb
}
\usage{
query.bety.con()
}
%- maybe also 'usage' for other objects documented here.
\details{
This function requires a file in $HOME/.my.cnf that allows user to
automatically log in to BETYdb. This file is used to specify the
database name, <username>, and <password>.
The contents of the file should be:
\code{
  [ebi_analysis]
  user = <username>
  password = <password>
  host = localhost
  database = ebi_analysis
}
\value{
Returns an object of class 'MySQLConnection' with a connection to
BETYdb that can be used to send queries and direct commands, depending
on user, to the database. 
}
\author{D.~S. LeBauer}
%\note{ ~~further notes~~ 
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{uses \code{\link{dbConnect}}, used by \code{\link{query.bety.traits}},\code{\link{query.bety.priors}},\code{\link{query.bety.species}},\code{\link{query.bety.parameters}}~~~ }
\examples{
## query all SLA data from database
con <- query.bety.con()
query <- "select mean from traits where variable_id in (select id from
variables where name like 'SLA';"
sla.data <- fetch(dbSendQuery(con, query), n=-1) 
hist(sla.data)

## see BETYdb documentation for more examples of queries that can be
   used
## also see other query.bety.* functions listed under \code{seealso} for
   queries used by PECAn 

## The function is currently defined as
function () 
{
    lapply(dbListConnections(MySQL()), dbDisconnect)
    dvr <- dbDriver("MySQL")
    con <- dbConnect(dvr, group = "ebi_analysis")
    return(con)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
\keyword{database}

