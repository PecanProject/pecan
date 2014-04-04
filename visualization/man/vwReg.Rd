\name{vwReg}
\alias{vwReg}
\title{PEcAn worldmap}
\usage{
vwReg(formula, data, title = "", B = 1000, shade = TRUE,
  shade.alpha = 0.1, spag = FALSE, spag.color = "darkblue",
  mweight = TRUE, show.lm = FALSE, show.median = TRUE,
  median.col = "white", shape = 21, show.CI = FALSE, method = loess,
  bw = FALSE, slices = 200, palette = colorRampPalette(c("#FFEDA0",
  "#DD0000"), bias = 2)(20), ylim = NULL, quantize = "continuous",
  add = FALSE, ...)
}
\arguments{
  \item{B}{= number bootstrapped smoothers}

  \item{shade:}{plot the shaded confidence region?}

  \item{shade.alpha:}{should the CI shading fade out at the
  edges? (by reducing alpha; 0 = no alpha decrease, 0.1 =
  medium alpha decrease, 0.5 = strong alpha decrease)}

  \item{spag:}{plot spaghetti lines?}

  \item{spag.color:}{color of spaghetti lines}

  \item{mweight:}{should the median smoother be visually
  weighted?}

  \item{show.lm:}{should the linear regresison line be
  plotted?}

  \item{show.CI:}{should the 95% CI limits be plotted?}

  \item{show.median:}{should the median smoother be
  plotted?}

  \item{median.col:}{color of the median smoother}

  \item{shape:}{shape of points}

  \item{method:}{the fitting function for the spaghettis;
  default: loess}

  \item{bw}{= TRUE: define a default b&w-palette}

  \item{slices:}{number of slices in x and y direction for
  the shaded region. Higher numbers make a smoother plot,
  but takes longer to draw. I wouldn'T go beyond 500}

  \item{palette:}{provide a custom color palette for the
  watercolors}

  \item{ylim:}{restrict range of the watercoloring}

  \item{quantize:}{either "continuous", or "SD". In the
  latter case, we get three color regions for 1, 2, and 3
  SD (an idea of John Mashey)}

  \item{add:}{if add == FALSE, a new ggplot is returned. If
  add == TRUE, only the elements are returned, which can be
  added to an existing ggplot (with the '+' operator)}

  \item{...:}{further parameters passed to the fitting
  function, in the case of loess, for example, "span = .9",
  or "family = 'symmetric'"}
}
\value{
NULL plot as side effect
}
\description{
Visually weighted regression / Watercolor plots
}
\details{
Idea: Solomon Hsiang, with additional ideas from many blog
commenters
}
\examples{
# build a demo data set
set.seed(1)
x <- rnorm(200, 0.8, 1.2)
e <- rnorm(200, 0, 3)*(abs(x)^1.5 + .5) + rnorm(200, 0, 4)
e2 <- rnorm(200, 0, 5)*(abs(x)^1.5 + .8) + rnorm(200, 0, 5)
y <- 8*x - x^3 + e
y2 <- 20 + 3*x + 0.6*x^3 + e2
df <- data.frame(x, y, y2)
p1 <- vwReg(y~x, df, spag=TRUE, shade=FALSE)
p2 <- vwReg(y2~x, df, add=TRUE, spag=TRUE, shade=FALSE, spag.color="red", shape=3)
p3 <- p1 + p2
p3

y <- x + x^2 + runif(200, 0, 0.4)
vwReg(y ~ x, df, method=lm)
vwReg(y ~ x + I(x^2), df, method=lm)
}
\author{
Felix Schönbrodt
}

