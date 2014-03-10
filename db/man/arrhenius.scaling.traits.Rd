\name{arrhenius.scaling.traits}
\alias{arrhenius.scaling.traits}
\title{Function to apply Arrhenius scaling to 25 degC for temperature-dependent traits}
\usage{
arrhenius.scaling.traits(data, covariates, temp.covariates, new.temp = 25)
}
\arguments{
  \item{data}{the data to scale}

  \item{covariates}{the relevant covariates}

  \item{temp.covariates}{names of covariates used to adjust
  for temperature; if length > 1, order matters (first will
  be used preferentially)}

  \item{new.temp}{the reference temperature for the scaled
  traits. Curerntly 25 degC}
}
\description{

}
\author{
Carl Davidson, David LeBauer
}

