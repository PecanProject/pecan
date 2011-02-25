dfdth <- function(f, th, yrange, name) {
  f <- as.numeric(f)
  th <- as.numeric(th)
  X <- as.matrix(cbind(th^2,th,rep(1,3)))
  beta <- solve(X,f)
  
  ## m.quad = f'(th) = 2*a*x + b evaluated at x2
  ## f''(th) = 2*a
  m.quad <- 2*beta[1]*th[2]+beta[2]
  dfdth.terms <- list('dfdth1' = m.quad, 'dfdth2' = 2*beta[1])
  
  ## plot (th,f) at lcl, mean, ucl
  plot(th, f, type = 'b', main = name, ylim = yrange)

  ##  plot quadratic fit in red
  xseq <- seq(0.9*min(th),1.1*max(th),length=100) 
  fseq <- beta[1]*xseq^2 + beta[2]*xseq + beta[3]
  lines(xseq,fseq,col=2)

  ## plot tangent line as dashed red
  b.quad <- f[2]-m.quad*th[2]
  abline(b.quad,m.quad,col=2,lty=2)
  
  return(dfdth.terms)
}
