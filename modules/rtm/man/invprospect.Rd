\name{invprospect}
\alias{invprospect}
\title{Invert the PROSPECT family (PROSPECT-4,PROSPECT-5,PROSPECT-5B) of leaf radiative transfer models}
\usage{
  invprospect(refl, tran, model, method, strategy,
    threshold, cpus)
}
\arguments{
  \item{refl}{observed leaf reflectance data}

  \item{tran}{observed leaf transmittance data}

  \item{model}{version of PROSPECT model to invert.
  Options: 4,5,5B.  Default = 4}

  \item{method}{algorithm for finding the minimum of cost
  function between observed and modeled spectra (i.e.
  optimize the model parameters). Current options: DEoptim}

  \item{strategy}{DEoptim strategy (see DEoptim)}

  \item{threshold}{minimum threshold of the difference
  between observed and modeled spectra during optimization.
  Default = 0.0001}

  \item{cpus}{the number of cpus to use in parallel
  inversion of PROSPECT (optional)}
}
\value{
  output optimum set of leaf parameters
  (N,Cab,Car,Cbrown,Cw,Cm), rmse of the inversion, and
  associated modeled reflectance and transmittance
}
\description{
  Invert the PROSPECT family
  (PROSPECT-4,PROSPECT-5,PROSPECT-5B) of leaf radiative
  transfer models
}
\details{
  { Function to invert the PROSPECT family
  (PROSPECT-4,PROSPECT-5,PROSPECT-5B) of leaf radiative
  transfer models (RTMs) on measured reflectance and
  transmittance data.  This function is used for single
  spectra inversions. }
}
\author{
  Shawn Serbin
}

