% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/half_hour_downscale.R
\name{downscale_repeat_6hr_to_half_hrly}
\alias{downscale_repeat_6hr_to_half_hrly}
\title{Downscale repeat to half hourly}
\usage{
downscale_repeat_6hr_to_half_hrly(df, varName, hr = 0.5)
}
\arguments{
\item{df}{dataframe of data to be downscaled (Longwave)}

\item{varName}{variable names to be downscaled}

\item{hr}{hour to downscale to- default is 0.5}
}
\value{
A dataframe of downscaled data
}
\description{
Downscale repeat to half hourly
}
\author{
Laura Puckett
}
