\name{summarize.GP}
\alias{summarize.GP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summarize.GP(gp, pdf_file = NULL, txt_file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gp}{ ~~Describe \code{gp} here~~ }
  \item{pdf_file}{ ~~Describe \code{pdf_file} here~~ }
  \item{txt_file}{ ~~Describe \code{txt_file} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(gp,pdf_file=NULL,txt_file=NULL){
  require("coda")
  nugget <- gp$nugget
  isotropic <- gp$isotropic
  d <- gp$d
  samp <- gp$samp
  if(is.null(pdf_file)){
    par(ask=TRUE)
  }else{
    pdf(pdf_file)
  }
  if(!is.null(txt_file)){sink(txt_file)}
  plot(gp$tauw)
  title("JUMP: TAUW")
  plot(gp$psi)
  title("JUMP: PSI")
  tauw <- mcmc(attr(gp$tauw,"chain")[samp,])
  psi <- mcmc(attr(gp$psi,"chain")[samp,])
  mu <- mcmc(gp$mu)
  if(nugget){
    tauv <- mcmc(gp$tauv)
    print("**** TAUV ****")
    summary(tauv)
    plot(tauv,main="TAUV")
    W <- mcmc(gp$W)
    print("**** W ****")
    summary(W)
    plot(W,main="W")
  }
  print("**** TAUW ****")
  print(summary(tauw))
  print("**** PSI ****")
  print(summary(psi))
  print("**** MU ****")
  print(summary(mu))
  ##par(ask=TRUE)
  plot(tauw)
  title("TAUW")
  plot(psi)
  title("PSI")
  plot(mu)
  title("MU")
  
  ## plot ACF
  par(mfrow=c(1,1))
  if(isotropic){
    xseq <- seq(0,max(d)/2,length=100)
    plot(xseq,mean(tauw)*exp(-mean(psi)*xseq^2),type='l')
  } else {
    ##anisotropic
    rng <- 0; for(i in 1:dim) rng <- max(c(rng,sqrt(max(d[[i]]))))
    xseq <- seq(0,rng/2,length=100)
    acorr <- matrix(NA,100,dim)
    for(k in 1:dim){
      acorr[,k] <- exp(-mean(psi[,k])*xseq^2)
    }
    plot(0,0,type='n',xlim=c(0,rng/2),ylim=c(0,max(acorr)),xlab="Parameter Distance",ylab="Correlation")
    for(k in 1:dim){
      lines(xseq,acorr[,k],col=k)
    }
  }
  par(ask=FALSE)
  if(!is.null(pdf_file)) dev.off()
  if(!is.null(txt_file)) sink()
  }  ##end summarize.GP
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
