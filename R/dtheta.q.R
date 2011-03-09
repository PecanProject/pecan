dtheta.q <- function(x, tr, q) {
  ## find the quantile of the mean
  q.mean <- mean(mean(x) >= x)
  ## find the quantiles of ucl and lcl (q.mean +/- 0.15)
  q.ucl  <- q.mean + q
  q.lcl  <- q.mean - q
  qs <- c(q.lcl, q.ucl, q.mean)
  if (tr != 'Vm_low_temp') {
    c(quantile(x,qs), var(x), sqrt(var(x))/mean(x))
  } else {
    c(quantile(x,qs), var(x), sqrt(var(x))/mean(x+273.15))
  }
}
