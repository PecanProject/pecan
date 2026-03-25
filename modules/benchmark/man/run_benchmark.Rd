\name{run_benchmark}
\alias{run_benchmark}
\title{Run a simple benchmark pipeline}
\usage{
run_benchmark(model_path, obs_path, metrics = c("RMSE", "MAE"), tolerance_secs = 3600)
}
\arguments{
\item{model_path}{path to model output CSV file}
\item{obs_path}{path to observations CSV file}
\item{metrics}{character vector of metrics to compute}
\item{tolerance_secs}{nearest-neighbor time tolerance in seconds}
}
\value{
list with: metrics (data.frame), aligned (data.frame), plot (ggplot)
}
\description{
Loads model output and observations, aligns by time,
computes RMSE and MAE, and returns a results table with a plot.
}
