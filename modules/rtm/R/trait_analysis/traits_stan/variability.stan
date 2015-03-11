data {
        int<lower=0> n_all;
        int<lower=1> nfe;
        matrix[n_all, nfe] X;
        vector[n_all] y_m;
        vector[n_all] y_s;
}

parameters {
        vector[nfe] Beta;
        real<lower=0> tau;
        vector[n_all] Y;
}

model {
        Y ~ normal(X * Beta, y_s);
        y_m ~ normal(Y, tau);

        Beta ~ normal(0,1000);
}

                

