functions {
        real expint(real k, vector e1, vector e2) {
                real xx;
                real yy;
                real tau;
                if(k <= 0) tau <- 1;
                if(k > 0 && k <= 4) {
                        xx <- 0.5 * k - 1.0;
                        yy <- ((((((((((((((((((
                        e1[1]*xx)+e1[2])*xx+e1[3])*xx+e1[4])*xx+
                        e1[5])*xx+e1[6])*xx+e1[7])*xx+e1[8])*xx+
                        e1[9])*xx+e1[10])*xx+e1[11])*xx+
                        e1[12])*xx+e1[13])*xx+e1[14])*xx+
                        e1[15])*xx+e1[16])*xx+e1[17]*xx+
                        e1[18])*xx+e1[19]);
                        yy <- yy-log(k);
                        tau <- (1.0-k)*exp(-k)+k^2*yy;
                };
                if(k > 4 && k <= 85) {
                        xx <- 14.5/(k+3.25)-1.0;
                        yy <- ((((((((((((((((((
                        e2[1]*xx)+e2[2])*xx+e2[3])*xx+e2[4])*xx+
                        e2[5])*xx+e2[6])*xx+e2[7])*xx+e2[8])*xx+
                        e2[9])*xx+e2[10])*xx+e2[11])*xx+
                        e2[12])*xx+e2[13])*xx+e2[14])*xx+
                        e2[15])*xx+e2[16])*xx+e2[17]*xx+
                        e2[18])*xx+e2[19])*xx+e2[20];
                        yy <- exp(-k)*yy/k;
                        tau <- (1.0-k)*exp(-k)+k^2*yy;
                };
                if(k > 85) tau <- 0;
                return tau;
        };

        vector prospect4(
                real<lower=1> N,
                real<lower=0> Cab,
                real<lower=0> Cw,
                real<lower=0> Cm,
                vector Cab_abs,        // Chlorophyll
                vector Cw_abs,         // Water
                vector Cm_abs,         // Dry matter

                vector tao1,
                vector tao2,
                vector rho1,
                vector rho2,
                vector x,
                vector y,
                vector e1,
                vector e2
                ){
                        vector[wl] k, theta;
                        k <- (1.0/N) * (Cab * Cab_abs +
                                        Cw * Cw_abs +
                                        Cm * Cm_abs);
                        theta <- expint(k, e1, e2);

                        vector[wl] rhoa, taoa, rho90, tao90;
                        rhoa <- rho1 + (tao1 * tao2 * rho2 * theta*theta) / (1 - rho2*rho2 * theta*theta);
                        taoa <- tao1 * tao2 * theta / (1 - rho2*rho2 * theta*theta);
                        rho90 <- (rhoa - y) / x;
                        tao90 <- taoa / x;

                        vector[wl] d90, a90, b90, nmR, dmRT, Refl;
                        d90 <- sqrt((tao90*tao90 - rho90*rho90 - 1.0)*(tao90*tao90 - rho90*rho90 - 1.0) - 4.0*rho90*rho90);
                        a90 <- (1.0 + rho90*rho90 - tao90*tao90 + d90) / (2.0*rho90);
                        b90 <- (1.0 - rho90*rho90 + tao90*tao90 + d90) / (2.0*tao90);
                        nmR <- taoa * tao90 * (b90^(N-1.0) - b90^(1.0-N));
                        dmRT <- a90*b90^(N-1.0) - b90^(1.0-N)/a90 - rho90 * (b90^(N-1.0) - b90^(1.0-N));
                        Refl <- rhoa + nmR / dmRT;

                        return Refl;
                };
}

data {
        int<lower=0> wl;    // number of wavelengths
        int<lower=1> nspec;     // number of spectra
        matrix[wl,nspec] observed;     // observation matrix

        vector[wl] Cab_abs;        // Chlorophyll
        vector[wl] Cw_abs;         // Water
        vector[wl] Cm_abs;         // Dry matter

        vector[wl] tao1;
        vector[wl] tao2;
        vector[wl] rho1;
        vector[wl] rho2;
        vector[wl] x;
        vector[wl] y;
        vector[19] e1;
        vector[20] e2;
}

parameters {
        real<lower=1> N;
        real<lower=0> Cab;
        real<lower=0> Cw;
        real<lower=0> Cm;
        real<lower=0> rsd;
}

model {
        // Priors
        log(N) ~ normal(0, 1000);
        log(Cab) ~ normal(0, 1000);
        log(Cw) ~ normal(0, 1000);
        log(Cm) ~ normal(0, 1000);
        rsd ~ inv_gamma(100, 100);

        // Model
        observed ~ normal(prospect4(N, Cab, Cw, Cm,
                                Cab_abs, Cw_abs, Cm_abs,
                                tao1, tao2, rho1, rho2,
                                e1, e2, x, y), rsd);
}
