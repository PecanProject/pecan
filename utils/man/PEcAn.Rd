\docType{package}
\name{PEcAn}
\alias{PECAn,}
\alias{PEcAn}
\alias{PEcAn-package}
\alias{package-pecan}
\alias{pecan,}
\title{R package to support PEcAn, the Predictive Ecosystem Analyzer}
\description{
Instructions for the use of this package are provided in
the project wiki
\url{http://128.174.125.122/wiki/index.php/PECAn_Documentation:Use}.
}
\details{
Project homepage: \url{pecanproject.org}

Description of PEcAn

The Predictive Ecosystem Analyzer (PEcAn) is a scientific
workflow management tool that is designed to simplify the
management of model parameterization, execution, and
analysis. The goal of PEcAn is to streamline the
interaction between data and models, and to improve the
efficacy of scientific investigation. PEcAn is an open
source utility that encapsulates:

1. acquisition of meteorological inputs 2. synthesis of
physiological trait data as the posterior distribution of a
Bayesian meta-analysis 3. sampling trait meta-analysis
posterior distributions to parameterize ensembles of ED2
and other ecophysiological models 4. probabilistic
forecasts 5. postprocessing to constrain forecasts and
model parameters with field, meterological, eddy flux, and
spectral data, and 6. provenance tracking

PECAn integrates available data into ecological forecasts
by running ensembles of a terrestrial ecosystem model that
is parameterized by the posterior distribution from a
meta-analysis of available plant trait data. These trait
data are assembled from field research and primary
literature, and are stored in a MySQL database. Current
development focused on biofuel crops uses BETYdb. In
addition to generating forecasts that reflect available
data, PEcAn quantifies the contribution of each parameter
to model uncertainty. This information informs targeted
data collection and synthesis efforts that most efficiently
reduce forecast uncertainty.

Current development is focused on developing PEcAn into a
real-time data assimilation and forecasting system. This
system will provide a detailed analysis of the past and
present ecosystem functioning that seamlessly transitions
into forecasts.
}

