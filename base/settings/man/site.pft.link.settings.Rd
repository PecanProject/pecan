% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/site_pft_link_settings.R
\name{site.pft.link.settings}
\alias{site.pft.link.settings}
\title{site.pft.link.settings}
\usage{
site.pft.link.settings(settings)
}
\arguments{
\item{settings}{settings list}
}
\value{
pecan xml setting file
}
\description{
This function reads in a pecan setting and check for the
pft.site xml tag under run>inputs. If a path or a ID for the input is
defined then, it will be used for linking sites with the pfts.
}
