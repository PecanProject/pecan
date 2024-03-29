% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_benchmark.R
\name{calc_benchmark}
\alias{calc_benchmark}
\title{Calculate benchmarking statistics}
\usage{
calc_benchmark(settings, bety, start_year = NA, end_year = NA)
}
\arguments{
\item{settings}{settings object describing the run to calculate}

\item{bety}{database connection}

\item{start_year, end_year}{time range to read. If NA, these are taken from `settings`}
}
\description{
For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
}
\author{
Betsy Cowdery
}
