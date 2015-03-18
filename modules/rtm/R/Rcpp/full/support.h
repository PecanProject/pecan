// Error between model and observations
NumericMatrix SpecError(
	NumericVector Model,
	NumericMatrix Observed,
	int nspec){
		NumericMatrix E(wl, nspec);
		int i,j;
		for(i=0; i<wl; i++){
			for(j=0; j<nspec; j++){
				E(i,j) = Model[i] - Observed(i,j);
			}
		}
		return E;
}

// Likelihood as a function of error matrix
double Likelihood(NumericMatrix E, double rsd, int nspec){
	double LogL;
	LogL = sum(Rcpp::dnorm(E, 0, rsd, true));
	return LogL;
}
	
			
