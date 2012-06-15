\name{trait.dictionary}
\alias{trait.dictionary}
\title{Dictionary of terms used to identify traits in ed, filenames, and figures}
\usage{
  trait.dictionary(traits = NULL)
}
\arguments{
  \item{traits}{a vector of trait names, if traits = NULL,
  all of the traits will be returned.}
}
\value{
  a dataframe with id, the name used by ED and BETY for a
  parameter; fileid, an abbreviated name used for files;
  figid, the parameter name written out as best known in
  english for figures and tables.
}
\description{
  Dictionary of terms used to identify traits in ed,
  filenames, and figures
}
\examples{
# convert parameter name to a string appropriate for end-use plotting
\dontrun{
trait.dictionary('growth_resp_factor')
trait.dictionary('growth_resp_factor')$figid

# get a list of all traits and units in dictionary
trait.dictionary()[,c('figid', 'units')]
}
}

