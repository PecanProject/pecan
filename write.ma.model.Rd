\name{write.ma.model}
\alias{write.ma.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write.ma.model(ma.model, con = "model.bug", pr.dist, pr.param.a, pr.param.b, total.n, total.trt, total.site)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ma.model}{
    this first argument points to a function, ma.model, that acts as a
    template for meta-analysis of different traits using different prior
    distributions and data. 
%%     ~~Describe \code{ma.model} here~~
}
\item{model.file}{
  String, name of the file in which the model will stored.
%%     ~~Describe \code{con} here~~
}
\item{pr.dist}{
  The root name used by R for the distribution used by the prior. Can
  be 'norm' for normal, 'lnorm' for log-normal, 'beta', 'unif', 'gamma',
  or any other distribution root recognized by R. Currently this
  meta-analysis model only accepts prior distributions with two parameters.
%%     ~~Describe \code{pr.dist} here~~
}
\item{pr.param.a}{
  This is the first of the two parameters, the definition will depend on
  the R implementation of the particular distribution defined by
  pr.dist. For examples, see the help pages for the relevant
  distribution (e.g. ?rnorm)
%%     ~~Describe \code{pr.param.a} here~~
}
\item{pr.param.b}{
  This is the second of the two parameters, the definition will depend on
  the R implementation of the particular distribution defined by
  pr.dist. For examples, see the help pages for the relevant
  distribution (e.g. ?rnorm)
%%     ~~Describe \code{pr.param.b} here~~
}
\item{total.n}{
   is the total number of data points in the data set. 
%%     ~~Describe \code{total.n} here~~
}
\item{total.trt}{
  total.trt is the total number of unique treatments in the data set.
%%     ~~Describe \code{total.trt} here~~
}
\item{total.site}{
   the total number of unique sites in the data set (e.g. total.site = length(unique(data$site))). 
%%     ~~Describe \code{total.site} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
  name <- 'SLA'
  jag.model.file <-  paste( name, ".model.bug",sep="")  # file to store model
  write.ma.model ( ma.model, jag.model.file,
                    prior$distn, prior$a, prior$b,
                    length ( data$Y ), length(unique(data$trt)), length(unique(data$site)))


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
