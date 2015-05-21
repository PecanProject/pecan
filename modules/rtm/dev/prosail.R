## Simple wrapper for PROSAIL
#setwd("../src/RTM")
#dyn.load("RTM.so")

prosp.def <- list(1.5, 40, 8, 0, 0.01, 0.009)

sail.def = c(prosp.def,
			 list(-0.35, -0.15, as.integer(1), 
			 	 3, 0.01,
			 	 30, 10, 0,
			 	 1))


prospect_5b_sail <- function(params = prosp.def){
	r <- matrix(0,nrow=2101,ncol=2)
	p <- c(list("PROSPECT_5B"), params, list(r))
	f <- do.call(.Fortran,p)
	out <- f[[7]]
	return(out)
}


pro4sail <- function(params = sail.def){
	r <- numeric(2101)
	p <- c(list("PRO4SAIL"), params, rep(list(r),4))
	lp <- length(p)-1
	f <- do.call(.Fortran, p)
	out <- do.call(cbind, f[(lp-3):lp])
	return(out)
}

prosp <- prospect_5b_sail()
plot(prosp[,1], type='l', ylim=c(0,1))
lines(1-prosp[,2], col=2)
title("PROSPECT output")

sail <- pro4sail()
plot(sail[,1], type='l')
lines(sail[,2], col=2)
lines(sail[,3], col=3)
lines(sail[,4], col=4)
title("SAIL output")

