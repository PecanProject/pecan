\name{zero.bounded.density}
\alias{zero.bounded.density}
\title{Zero Bounded Density}
\usage{
zero.bounded.density(x, bw = "SJ", n = 1001)
}
\arguments{
  \item{x}{}

  \item{bw}{The smoothing bandwidth to be used. See
  'bw.nrd'}
}
\value{
data frame with back-transformed log density estimate
}
\description{
Zero bounded density using log density transform
}
\details{
Provides a zero bounded density estimate of a parameter.
Kernel Density Estimation used by the
\code{\link{stats::density}} function will cause problems
at the left hand end because it will put some weight on
negative values. One useful approach is to transform to
logs, estimate the density using KDE, and then transform
back.
}
\author{
\href{http://stats.stackexchange.com/q/6588/2750}{Rob
Hyndman}
}
\references{
M. P. Wand, J. S. Marron and D. Ruppert, 1991.
Transformations in Density Estimation. Journal of the
American Statistical Association. 86(414):343-353
\url{http://www.jstor.org/stable/2290569}
}

