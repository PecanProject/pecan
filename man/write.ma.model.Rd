\name{write.ma.model}
\alias{write.ma.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creating the PECAn meta-analysis model file
%%  ~~function to do ... ~~
}
\description{
  Convert the R function ma.model to a JAGS model.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write.ma.model(ma.model, con = "model.bug", pr.dist, pr.param.a, pr.param.b, total.n, total.trt, total.site)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ma.model}{R function containing the JAGS model in teh JAGS model
    language, with text substitutions that insert the prior distribution
    and other model inputs that vary among specific analyses.
}
  \item{con}{passed to \code{writeLines} which actually writes the model file.
}
  \item{pr.dist}{A string representing the root distribution name used
    by R, e.g. 'norm', 'lnorm', 'gamma', 'beta', etc.
}
  \item{pr.param.a}{The first parameter value accepted by \code{pr.dist}
}
  \item{pr.param.b}{The second parameter value for the \code{pr.dist}
}
  \item{total.n}{The total number of observations in the data
}
  \item{total.trt}{The total number of treatments in the data
}
  \item{total.site}{The total number of sites in the data
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Nothing, but as a side effect, the model is written
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{David LeBauer and Mike Dietze, based on original work on the
  \code{write.model} function in the \code{R2WinBUGS} package by Jouni
  Kerman and Uwe Ligges.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ma.model, con = "model.bug", pr.dist,
pr.param.a, pr.param.b, total.n, total.trt, total.site){

 if (is.R()) {
   model.text <- attr(ma.model, "source")
   if(!is.null(model) & is.null(model.text)){
     ## running from cmd line, attr doesn't work
     sink(".temp.jags")
     print(ma.model)
     sink()

     model.text <- scan(file=".temp.jags",what="character",sep="@")
             ## chose an uncommon separator in order to capture whole lines

   }
   model.text <- sub("^\\s*function\\s*\\(\\s*\\)", "ma.model",
                     model.text)
 } else {
   model.text <- as.character(ma.model)
   model.text <- paste("model", model.text)
 }
 model.text <- gsub("\%.\%", "", model.text)
 model.text <- gsub("PRIORDIST", paste("d", pr.dist,sep=""), model.text)
 model.text <- gsub("PRIORPARAMA", pr.param.a, model.text)
 model.text <- gsub("PRIORPARAMB", pr.param.b, model.text)
 model.text <- gsub("LENGTHK", total.n, model.text)
 model.text <- gsub("LENGTHJ", total.trt, model.text)
 model.text <- gsub("LENGTHG", total.site, model.text)
 if (!is.R()) {
   model.text <- replaceScientificNotation(model.text)
   model.text <- gsub("invisible[ ]*\\([ ]*\\)", "", model.text)
 }

 writeLines(model.text, con = con)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
