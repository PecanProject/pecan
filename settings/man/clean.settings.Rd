\name{clean.settings}
\alias{clean.settings}
\title{Cleans PEcAn settings file}
\usage{
  clean.settings(inputfile = "pecan.xml",
    outputfile = "pecan.xml")
}
\arguments{
  \item{inputfile}{the PEcAn settings file to be used.}

  \item{outputfile}{the name of file to which the settings
  will be written inside the outputdir.}
}
\value{
  list of all settings as saved to the XML file(s)
}
\description{
  This will try and clean the settings file so it is ready
  for a new run. This will remove all run specific
  information and set the outdir to be "pecan" for the next
  run.
}
\examples{
\dontrun{
clean.settings("output/PEcAn_1/pecan.xml", "pecan.xml")
}
}
\author{
  Rob Kooper
}

