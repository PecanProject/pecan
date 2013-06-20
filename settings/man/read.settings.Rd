\name{read.settings}
\alias{read.settings}
\title{Loads PEcAn settings file}
\usage{
  read.settings(inputfile = NULL, outputfile = "pecan.xml")
}
\arguments{
  \item{inputfile}{the PEcAn settings file to be used.}

  \item{outputfile}{the name of file to which the settings
  will be written inside the outputdir.}
}
\value{
  list of all settings as loaded from the XML file(s)
}
\description{
  This will try and find the PEcAn settings file in the
  following order:
}
\details{
  \enumerate{ \item {--settings <file>}{passed as command
  line argument using --settings} \item {inputfile}{passed
  as argument to function} \item
  {PECAN_SETTINGS}{environment variable PECAN_SETTINGS
  pointing to a specific file} \item
  {./pecan.xml}{pecan.xml in the current folder} }
}
\examples{
\dontrun{
settings <- read.settings()
settings <- read.settings(file="willowcreek.xml")
test.settings.file <- system.file("tests/test.xml", package = "PEcAn.all")
settings <- read.settings(test.settings.file)
}
}
\author{
  Shawn Serbin

  Rob Kooper
}

