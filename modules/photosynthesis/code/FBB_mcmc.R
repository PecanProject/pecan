### MCMC Implementation

# Space for tracked variables
if(sample.Vcmax) 	{ Vcmax.mcmc	= matrix(NA, nrow=niter, ncol=nchains) }
if(sample.Jmax) 	{ Jmax.mcmc 	= matrix(NA, nrow=niter, ncol=nchains) }
if(sample.R) 		{ R.mcmc  		= matrix(NA, nrow=niter, ncol=nchains) }
if(sample.Gstar) 	{ Gstar.mcmc  	= matrix(NA, nrow=niter, ncol=nchains) }
if(sample.alpha)	{ alpha.mcmc   	= matrix(NA, nrow=niter, ncol=nchains) }
if(sample.m) 		{ m.mcmc   		= matrix(NA, nrow=niter, ncol=nchains) }
if(sample.g0) 		{ g0.mcmc   	= matrix(NA, nrow=niter, ncol=nchains) }
if(sample.tauBB) 	{ tauBB.mcmc   	= matrix(NA, nrow=niter, ncol=nchains) }
if(sample.tauF) 	{ tauF.mcmc   	= matrix(NA, nrow=niter, ncol=nchains) }
if(track.An.pred){An.pred.mcmc 	= matrix(NA, nrow=niter*nchains, ncol=npoints) }
if(track.gs.pred){gs.pred.mcmc 	= matrix(NA, nrow=niter*nchains, ncol=npoints) }
if(compute.DA) 		{ DA.mcmc = matrix(NA, nrow=niter, ncol=nchains) }
if(compute.Dgs) 	{ Dgs.mcmc = matrix(NA, nrow=niter, ncol=nchains) }
if(compute.pA) 		{ pA.mcmc  = matrix(NA, nrow=niter*nchains, ncol=npoints) }
if(compute.pgs) 	{ pgs.mcmc  = matrix(NA, nrow=niter*nchains, ncol=npoints) }

my.model = solve.model(Vcmax, Jmax, R, Gstar, alpha, m, g0)
	
## MCMC loop
for(b in 1:niter) {
	
	# Vcmax - Metropolis-Hastings Sampling
	if(sample.Vcmax){
			Vcmax.new = jump.Vcmax(Vcmax)						# jump to new Vcmax
			my.model.new = solve.model(						# solve coupled F & BB
				Vcmax.new, Jmax, R, Gstar, alpha, m, g0)
			p.Vcmax.new = llik.A(my.model.new$An.pred,tauF) + 	# new A liklihood
				llik.gs(my.model.new$gs.pred,tauBB) + 			# new gs liklihood
				prior.Vcmax(Vcmax.new)							# new prior
			p.Vcmax.old = llik.A(my.model$An.pred,tauF) + 		# old A liklihood
				llik.gs(my.model$gs.pred,tauBB) + 				# old gs liklihood
				prior.Vcmax(Vcmax)								# old prior
			accept = exp(p.Vcmax.new - p.Vcmax.old)		# compute acceptrance ratio
			if(accept > runif(1)) {					# check for acceptance
	    		Vcmax = Vcmax.new					# replace Vcmax
	    	   	my.model = my.model.new				# replace A.pred & gs.pred
			}
			Vcmax.mcmc[b,chain] = Vcmax			# save result of this iteration
	} # End if(sample.Vcmax)
	
	
	# Jmax - Metropolis-Hastings Sampling
	if(sample.Jmax){
			Jmax.new = jump.Jmax(Jmax)						# jump to new Jmax
			my.model.new = solve.model(						# solve coupled F & BB
				Vcmax, Jmax.new, R, Gstar, alpha, m, g0)
			p.Jmax.new = llik.A(my.model.new$An.pred,tauF) + 	# new A liklihood
				llik.gs(my.model.new$gs.pred,tauBB) + 			# new gs liklihood
				prior.Jmax(Jmax.new)							# new prior
			p.Jmax.old = llik.A(my.model$An.pred,tauF) + 		# old A liklihood
				llik.gs(my.model$gs.pred,tauBB) + 				# old gs liklihood
				prior.Jmax(Jmax)								# old prior
			accept = exp(p.Jmax.new - p.Jmax.old)		# compute acceptrance ratio
			if(accept > runif(1)) {						# check for acceptance
	    		Jmax = Jmax.new							# replace Jmax
	    	   	my.model = my.model.new					# replace A.pred & gs.pred
			}
			Jmax.mcmc[b,chain] = Jmax			# save result of this iteration
	} # End if(sample.Jmax)
	
	
	# R - Metropolis-Hastings Sampling
		if(sample.R){
			R.new = jump.R(R)								# jump to new R
			my.model.new = solve.model(						# solve coupled F & BB
				Vcmax, Jmax, R.new, Gstar, alpha, m, g0)
			p.R.new = llik.A(my.model.new$An.pred,tauF) + 	# new A liklihood
				llik.gs(my.model.new$gs.pred,tauBB) + 		# new gs liklihood
				prior.R(R.new)								# new prior
			p.R.old = llik.A(my.model$An.pred,tauF) + 		# old A liklihood
				llik.gs(my.model$gs.pred,tauBB) + 			# old gs liklihood
				prior.R(R)									# old prior
			jnew  = dtnorm(R.new,R,jumpSD.R,0,100)			# J(new|current)
			jold  = dtnorm(R,R.new,jumpSD.R,0,100)			# J(current|new)
			accept = exp((p.R.new-jnew) - (p.R.old-jold))		# compute acceptrance 
			if(accept > runif(1)) {					# check for acceptance
	    		R = R.new							# replace R
	    	   	my.model = my.model.new				# replace A.pred & gs.pred
			}
			R.mcmc[b,chain] = R			# save result of this iteration
	} # End if(sample.R)
	
	
	# Gstar - Metropolis-Hastings Sampling
		if(sample.Gstar){
			Gstar.new = jump.Gstar(Gstar)					# jump to new Gstar
			my.model.new = solve.model(						# solve coupled F & BB
				Vcmax, Jmax, R, Gstar.new, alpha, m, g0)
			p.Gstar.new = llik.A(my.model.new$An.pred,tauF) + 	# new A liklihood
				llik.gs(my.model.new$gs.pred,tauBB) + 			# new gs liklihood
				prior.Gstar(Gstar.new)							# new prior
			p.Gstar.old = llik.A(my.model$An.pred,tauF) + 		# old A liklihood
				llik.gs(my.model$gs.pred,tauBB) + 				# old gs liklihood
				prior.Gstar(Gstar)								# old prior
			accept = exp(p.Gstar.new - p.Gstar.old)		# compute acceptrance ratio
			if(accept > runif(1)) {					# check for acceptance
	    		Gstar = Gstar.new					# replace Gstar
	    	   	my.model = my.model.new				# replace A.pred & gs.pred
			}
			Gstar.mcmc[b,chain] = Gstar		# save result of this iteration
	} # End if(sample.Gstar)
	
	
	# alpha - Metropolis-Hastings Sampling
		if(sample.alpha){
			alpha.new = jump.alpha(alpha)					# jump to new alpha
			my.model.new = solve.model(						# solve coupled F & BB
				Vcmax, Jmax, R, Gstar, alpha.new, m, g0)
			p.alpha.new = llik.A(my.model.new$An.pred,tauF) + 	# new A liklihood
				llik.gs(my.model.new$gs.pred,tauBB) + 			# new gs liklihood
				prior.alpha(alpha.new)							# new prior
			p.alpha.old = llik.A(my.model$An.pred,tauF) + 		# old A liklihood
				llik.gs(my.model$gs.pred,tauBB) + 				# old gs liklihood
				prior.alpha(alpha)								# old prior
			jnew  = dtnorm(alpha.new,alpha,jumpSD.alpha,0,1)	# J(new|current)
			jold  = dtnorm(alpha,alpha.new,jumpSD.alpha,0,1)	# J(current|new)
			accept = exp((p.alpha.new-jnew) - (p.alpha.old-jold))	# compute acceptrance 
			if(accept > runif(1)) {					# check for acceptance
	    		alpha = alpha.new					# replace alpha
	    	   	my.model = my.model.new				# replace A.pred & gs.pred
			}
			alpha.mcmc[b,chain] = alpha		# save result of this iteration
	} # End if(sample.alpha)
	
	
	# m - Metropolis-Hastings Sampling
	if(sample.m){
			m.new = jump.m(m)								# jump to new m
			my.model.new = solve.model(						# solve coupled F & BB
				Vcmax, Jmax, R, Gstar, alpha, m.new, g0)
			p.m.new = llik.A(my.model.new$An.pred,tauF) + 	# new A liklihood
				llik.gs(my.model.new$gs.pred,tauBB) + 		# new gs liklihood
				prior.m(m.new)								# new prior
			p.m.old = llik.A(my.model$An.pred,tauF) + 		# old A liklihood
				llik.gs(my.model$gs.pred,tauBB) + 			# old gs liklihood
				prior.m(m)									# old prior
			accept = exp(p.m.new - p.m.old)				# compute acceptrance ratio
			if(accept > runif(1)) {					# check for acceptance
	    		m = m.new							# replace m
	    	   	my.model = my.model.new				# replace A.pred & gs.pred
			}
			m.mcmc[b,chain] = m				# save result of this iteration
	} # End if(sample.m)
	
	
	# g0 - Metropolis-Hastings Sampling
		if(sample.g0){
			g0.new = jump.g0(g0)								# jump to new g0
			my.model.new = solve.model(						# solve coupled F & BB
				Vcmax, Jmax, R, Gstar, alpha, m, g0.new)
			p.g0.new = llik.A(my.model.new$An.pred,tauF) + 	# new A liklihood
				llik.gs(my.model.new$gs.pred,tauBB) + 		# new gs liklihood
				prior.g0(g0.new)								# new prior
			p.g0.old = llik.A(my.model$An.pred,tauF) + 		# old A liklihood
				llik.gs(my.model$gs.pred,tauBB) + 			# old gs liklihood
				prior.g0(g0)									# old prior
			accept = exp(p.g0.new - p.g0.old)			# compute acceptrance ratio
			if(accept > runif(1)) {					# check for acceptance
	    		g0 = g0.new							# replace g0
	    	   	my.model = my.model.new				# replace A.pred & gs.pred
			}
			g0.mcmc[b,chain] = g0				# save result of this iteration
	} # End if(sample.g0)
	
	
	# tauF - Gibbs Sampling
	if(sample.tauF){
			tauF = gibbs.tauF(my.model$An.pred,tauF)
			tauF.mcmc[b,chain] = tauF			# save result of this iteration
	}
	
	
	# tauBB - Gibbs Sampling
	if(sample.tauBB){
			tauBB = gibbs.tauBB(my.model$gs.pred,tauBB)
			tauBB.mcmc[b,chain] = tauBB		# save result of this iteration	
	}
	
	
	
	## Compute PI
	if(compute.pA) { 
		pA.mcmc[b+niter*(chain-1),] = rnorm(npoints,my.model$An.pred,sqrt(tauF)) 
		}
	
	if(compute.pgs) { 
		pgs.mcmc[b+niter*(chain-1),] = rnorm(npoints,my.model$gs.pred,sqrt(tauBB)) 
		}
	
	
	## Compute D (liklihood of data|params)
	if(compute.DA) { DA.mcmc[b,chain] = -2*llik.A(my.model$An.pred,tauF) }
	
	if(compute.Dgs) { Dgs.mcmc[b,chain] = -2*llik.gs(my.model$gs.pred,tauBB) }
	
	
	# save data model values
	An.pred.mcmc[b+niter*(chain-1),] = my.model$An.pred			# save result of this iteration
	gs.pred.mcmc[b+niter*(chain-1),] = my.model$gs.pred			# save result of this iteration


	# Print Progress
	if(b %% progressEvery == 0) { 
		print(paste("Species:",species.id,"Leaf:",leaf," Chain:",chain," Iterations:",b)) 
		}
}