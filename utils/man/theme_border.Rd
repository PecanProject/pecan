\name{theme_border}
\alias{theme_border}
\title{Theme border for plot}
\usage{
theme_border(type = c("left", "right", "bottom", "top", "none"),
  colour = "black", size = 1, linetype = 1)
}
\arguments{
  \item{type}{}

  \item{colour}{}

  \item{size}{}

  \item{linetype}{}
}
\value{
adds borders to ggplot as a side effect
}
\description{
Add borders to .. content for \description{} (no empty
lines) ..
}
\details{
Has ggplot2 display only specified borders, e.g.
("L"-shaped) borders, rather than a rectangle or no border.
Note that the order can be significant; for example, if you
specify the L border option and then a theme, the theme
settings will override the border option, so you need to
specify the theme (if any) before the border option, as
above.
}
\examples{
\dontrun{
df = data.frame( x=c(1,2,3), y=c(4,5,6) )
ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() +
       opts(panel.border = theme_border(c("bottom","left")) )
ggplot(data=df, aes(x=x, y=y)) + geom_point() + theme_bw() +
       opts(panel.border = theme_border(c("b","l")) )
}
}
\author{
Rudolf Cardinal

\url{ggplot2 google
group}{https://groups.google.com/forum/?fromgroups#!topic/ggplot2/-ZjRE2OL8lE}
}

