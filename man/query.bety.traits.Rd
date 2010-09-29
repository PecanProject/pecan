\name{query.bety.traits}
\alias{query.bety.traits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
query.bety.traits()
}
%- maybe also 'usage' for other objects documented here.
\details{
  %%  ~~ If necessary, more details than the description above ~~
  After querying the data, the function then converts multiple statistics
to standard error, $SE$, an estimate of the population standard
deviation ($\hat{\sigma}$). When necessary, for example when information
is unavailable, these equations provide conservative (inflated) values of $\hat{\sigma}$.
In the context of the model that I am using, an overestimate of variance is okay.

Below is the list of transformations to $SE$, where
$SE=\sqrt{MSE/n}$. These calculations assume the following:
\begin{itemize}
\item $\alpha=0.05$ thus $1-^{\alpha}/_2=0.975$
\item variables are normally distributed unless otherwise stated
\end{itemize}

\begin{enumerate}

\item given $P$, $n$, and treatment means $\bar X_1$ and $\bar X_2$ 
$$SE=\frac{\bar X_1-\bar X_2}{t_{(1-\frac{P}{2},n)}}$$ 

\item given LSD, $\alpha$, $n$, $b$ where $b$ is
number of blocks, and $n=b$ by default for RCBD  \cite{rosenberg2004map,saville2003bsi}
$$SE = \frac{LSD}{t_{(0.975,n)}\sqrt{2bn}}$$

\item given MSD (minimum significant difference) \cite{wang2000asp}, $n$, $\alpha$, df = $2n-2$
$$SE = \frac{MSD}{t_{(0.975, 2n-2)}\sqrt{2}}$$

\item given a 95 \% Confidence Interval [1] (measured from mean to upper or lower confidence limit), $\alpha$, and $n$

$$SE = \frac{CI}{t_{(0.975,n)}}$$

\item given Tukey's HSD, $n$, where $q$ is the 'studentized range statistic',

$$SE = \frac{HSD}{q_{(0.975,n)}}$$

\end{enumerate}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


}
 
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
