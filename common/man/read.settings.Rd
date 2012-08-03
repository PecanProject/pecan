\name{read.settings}
\alias{read.settings}
\title{Loads PEcAn settings file}
\usage{
  read.settings(inputfile = NULL, outputfile = "pecan.xml")
}
\arguments{
  \item{inputfile}{the PEcAn settings file to be merged
  with the others.}

  \item{outputfile}{the name of file to which the settings
  will be written inside the outputdir.}
}
\value{
  list of all settings as loaded from the XML file(s)
}
\description{
  This will load the PEcAn settings file in the following
  order, merging their values and overriding any values
  that are specified in a file later in the order
}
\details{
  \enumerate{ \item {/etc/pecan.xml}{global file for all
  users} \item {~/.pecan.xml}{settings for all projects for
  the user} \item {PECAN_SETTINGS}{environment variable
  PECAN_SETTINGS pointing to a specific file} \item
  {inputfile}{passed as argument to function} \item
  {--settings <file>}{passed as command line argument using
  --settings} }
}
\examples{
\dontrun{
settings <- read.settings()
settings <- read.settings(file="willowcreek.xml")
test.settings.file <- system.file("tests/test.settings.xml", package = "PEcAn.all")
settings <- read.settings(test.settings.file)
}
}
\author{
  Shawn Serbin

  Rob Kooper
}

