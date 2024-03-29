% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_BRR.R
\name{create_BRR}
\alias{create_BRR}
\title{Create benchmark reference run and ensemble}
\usage{
create_BRR(ens_wf, con, user_id = "")
}
\arguments{
\item{ens_wf}{table made from joining ensemble and workflow tables}

\item{con}{database connection}

\item{user_id}{Optional user id to use for this record in reference_runs table}
}
\description{
For each benchmark id, calculate metrics and update benchmarks_ensemble_scores
}
\author{
Betsy Cowdery
}
