// Priors
double dnorm_simp(double x, double mu, double sd){
	double d, pi;
	pi = 3.141592653590;
	d = 1.0/(sd*sqrt(2*pi))*exp(-(x-mu)*(x-mu)/(2*sd*sd));
	return log(d);
}

double priorN(double logN){
	double p;
	p = dnorm_simp(logN, -0.916, 2.2);
	return p;
}

double priorCab(double logCab){
	double p;
	p = dnorm_simp(logCab, 3.4, 0.9);
	return p;
}

double priorCw(double logCw){
	double p;
	p = dnorm_simp(logCw, -6.377, 0.5);
	return p;
}

double priorCm(double logCm){
	double p;
	p = dnorm_simp(logCm, -5.116, 0.9);
	return p;
}
	