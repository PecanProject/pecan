### FBB_funtions
## Required library
library(MCMCpack)

## truncated normal density
dtnorm = function(x,mu,sd,minX,maxX) {
	dnorm(x,mu,sd,log=TRUE) - log(pnorm(maxX,mu,sd) - pnorm(minX,mu,sd)) }

## truncated normal random number
rtnorm = function(n,mu,sd,minX,maxX) {
	x = rnorm(n,mu,sd)
	redo = which(x<minX | x>maxX)
	while(length(redo)>0) {
		x[redo] = rnorm(length(redo),mu,sd)
		redo = which(x<minX | x>maxX) }
	return(x) }


## likelihood of A
llik.A = function(An.pred, tauF) {
		sum(dnorm(An.obs, An.pred, sqrt(tauF), log=TRUE)) }


## likelihood of gs
llik.gs = function(gs.pred, tauBB) {							
		sum(dnorm(gs.obs, gs.pred, sqrt(tauBB), log=TRUE)) }


## Prior Distributions
prior.Vcmax = function(Vcmax)	{ dlnorm(Vcmax, prior.mu.Vcmax, prior.sd.Vcmax, log=TRUE) }
prior.Jmax 	= function(Jmax)	{ dlnorm(Jmax, prior.mu.Jmax, prior.sd.Jmax, log=TRUE) }
prior.R 	= function(R)		{ dlnorm(R, prior.mu.R, prior.sd.R, log=TRUE) }
prior.Gstar = function(Gstar)	{ dlnorm(Gstar, prior.mu.Gstar, prior.sd.Gstar, log=TRUE) }

#prior.alpha = function(alpha)	{ dunif(alpha,0,1) }
prior.alpha = function(alpha)	{ dlnorm(alpha, prior.mu.alpha, prior.sd.alpha, log=TRUE) }

prior.m 	= function(m)		{ dnorm(m, prior.mu.m, prior.sd.m, log=TRUE) }
prior.g0 	= function(g0)		{ dlnorm(g0, prior.mu.g0, prior.sd.g0, log=TRUE) }


## Jump Distributions for Metropolis Sampled Paramters
jump.Vcmax 	= function(Vcmax) 	{ rnorm(1,Vcmax,jumpSD.Vcmax) }		
jump.Jmax 	= function(Jmax) 	{ rnorm(1,Jmax,jumpSD.Jmax) }			
jump.R 		= function(R) 		{ rtnorm(1,R,jumpSD.R,R.lb,R.ub) }		
jump.Gstar 	= function(Gstar) 	{ rnorm(1,Gstar,jumpSD.Gstar) }			
jump.alpha 	= function(alpha) 	{ rtnorm(1,alpha,jumpSD.alpha,alpha.lb,alpha.ub) }
jump.m 		= function(m) 		{ rnorm(1,m,jumpSD.m) }
#jump.g0 	= function(g0) 		{ rnorm(1,g0,jumpSD.g0) }
jump.g0 	= function(g0) 		{ rtnorm(1,g0,jumpSD.g0,g0.lb,g0.ub) }

## Gibbs Sampling for Variances
gibbs.tauF = function(An.pred,tauF){
	shape = prior.s1.tauF+length(An.pred)/2
	rate = prior.s2.tauF+0.5*sum((An.pred-An.obs)^2)
	rinvgamma(1,shape,rate) }

gibbs.tauBB = function(gs.pred,tauBB){
	shape = prior.s1.tauBB+length(gs.pred)/2
	rate = prior.s2.tauBB+0.5*sum((gs.pred-gs.obs)^2)
	rinvgamma(1,shape,rate) }
	

## Farquhar-Ball Berry Optimization Functions
farquhar = function(Ci,Fparams,Q){
	Q2 <- Q*Fparams[5]*phi*beta
	J <- (Q2+Fparams[2]-sqrt((Q2+Fparams[2])*(Q2+Fparams[2])-4*theta*Q2*Fparams[2]))/(2*theta)
	Ac <- Fparams[1]*(Ci-Fparams[4])/(Ci+(Kc*(1+(O/Ko))))
	Aj <- J*(Ci-Fparams[4])/((4.5*Ci)+(10.5*Fparams[4]))
	min(Aj,Ac) - Fparams[3]
}


ballberry = function(input,BBparams,Fparams,obs){
	Ci <- obs[1] - input[1]/input[2]
	e1 <- farquhar(Ci,Fparams,obs[3]) - input[1]
	e2 <- (BBparams[1] + BBparams[2]*input[1]*obs[2]/obs[1] - input[2])*100
	return(e1^2 + e2^2)
}


solve.model = function(Vcmax, Jmax, R, Gstar, alpha, m, g0){
	output = list()
	for(i in 1:npoints){					# loop over data points
		ic <- c(An.obs[i], gs.obs[i]) 		# take initial conditions from actual data
		out <- optim(ic,			# solve simultaneously for An.pred and gs.pred
				ballberry,
				BBparams = c(g0,m),	        # Ballberry params                                                                                                                                                                                                                                                                                                            
				Fparams = c(Vcmax,Jmax,R,Gstar,alpha),	# Farquhar params
				obs = c(Ca[i], H[i], Q[i]))  			# data
		output$An.pred[i] = out$par[1]
		output$gs.pred[i] = out$par[2]
	}	
	return(output)
}
