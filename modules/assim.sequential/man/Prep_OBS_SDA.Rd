% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Prep_OBS_SDA.R
\name{Prep_OBS_SDA}
\alias{Prep_OBS_SDA}
\title{SDA observation preparation function for LAI and AGB}
\usage{
Prep_OBS_SDA(settings, out_dir, AGB_dir, Search_Window = 30)
}
\arguments{
\item{settings}{multi.settings objects that contains multiple sites info}

\item{out_dir}{output dir}

\item{AGB_dir}{AGB data dir}

\item{Search_Window}{search window for locate available LAI values}
}
\value{
mean and covariance of observations
}
\description{
SDA observation preparation function for LAI and AGB
}
