### Prospect Model
### Alexey Shiklomanov

## alpha = maximum incidence angle defining the solid angle Omega
## n = refractive index
## theta = transmission coefficient of the plate
## N = number of effective layers

# Auxiliary functions #####

exp.t <- function(x) exp(-x) / x
itg.k <- function(k) sapply(k, function(x) integrate(exp.t, x, Inf)$value)

# Generalized Plate Model #####

gpm <- function(alpha, n, theta, N){
  ## Transmittance of isotropic light through medium as a 
  ##  function of angle 'alpha' and refractive index 'n'
  t.av <- function(alpha, n){
    alpha <- alpha * pi/180
    
    np <- n^2 + 1
    nm <- n^2 - 1
    a <- ((n + 1)^2) / 2
    k <- (-(n^2 - 1)^2) / 4
    sa <- sin(alpha)
    
    if(alpha == 0) {
      out <- 4 * n / (n+1)^2
      return(out)
    } else if(alpha == pi/2) {
      b1 <- rep(0, length(n))
    } else {
      b1 <- sqrt((sa^2 - np/2)^2 + k)
    }
    
    b2 <- sa^2 - np/2
    b <- b1 - b2
    
    t.s <- (k^2/(6*b^3) + k/b - b/2) - (k^2/(6*a^3) + k/a - a/2)
    t.p1 <- -2*n^2 * (b - a) / np^2
    t.p2 <- -2*n^2 * np * log(b/a) / nm^2
    t.p3 <- n^2 * (1/b - 1/a) / 2
    t.p4 <- 16*n^4 * (n^4 + 1) * log((2*np*b - nm^2)/(2*np*a - nm^2)) / (np^3 * nm^2)
    t.p5 <- 16*n^6 * (1/(2*np*b - nm^2) - 1/(2*np*a - nm^2)) / np^3
    t.p <- t.p1 + t.p2 + t.p3 + t.p4 + t.p5
    
    out <- (t.s + t.p) / (2*sa^2)
    
    return(out)
  }
  t90 <- t.av(90, n)
  tav <- t.av(alpha, n)
  
  ## Reflectance of a plane as a function of incident angle 'alpha'
  rho.a <- function(alpha){
    (1 - tav) + 
      t90 * tav * theta^2 * (n^2 - t90) / 
      (n^4 - theta^2 * (n^2- t90)^2)  
  }
  rho90 <- rho.a(90)
  
  ## Transmittance through a plane as a function of incident angle 'alpha'
  tao.a <- function(alpha){
    t90 * tav * theta * n^2 / 
      (n^4 - theta^2 * (n^2 - t90)^2)
  }
  tao90 <- tao.a(90)
  
  d90 <- sqrt((t90^2 - rho90^2 - 1)^2 - 4*rho90^2)
  a90 <- (1 + rho90^2 - tao90^2 + d90) / (2*rho90)
  b90 <- (1 - rho90^2 + tao90^2 + d90) / (2*tao90)
  
  x <- tav / t90
  y <- x * (t90 - 1) + 1 - tav
  
  
  ### TODO - Fix to make this work with multiple layers. Try to figure out what Shawn did.
  R.N.90 <- b90^N - b90^(-N) / (a90*b90^N - a90^(-1)*b90^(-N))
  R.N.a <- x * R.N.90 + y
  
  T.N.90 <- a90 - a90^(-1) / (a90*b90^N - a90^(-1)*b90^(-N))
  T.N.a <- x * T.N.90
  return(data.frame(R = R.N.a, Tr = T.N.a, t90, tav, rho90, tao90))
}

prospect <- function(N, Cab, Cw, Cm,
                     data=dataSpec_p4, alpha=40.0, wavelengths=400:2500){
  n <- data$refractive_index      # Column 2
  
  k.cab <- Cab * data$specific_abs_coeff_chl    # Column 3
  k.w <- Cw * data$specific_abs_coeff_cw        # Column 5
  k.m <- Cm * data$specific_abs_coeff_cm        # Column 6
  k <- (k.cab + k.w + k.m) / N
  theta <- (1-k)*exp(-k) + k^2 * itg.k(k)
  
  rt <- gpm(alpha, n, theta, N)
  rt$wavelength <- wavelengths
  return(rt)
}

# Here are some examples observed during the LOPEX'93 experiment on
# fresh (F) and dry (D) leaves :
testdata <- data.frame(
  plant = c('min.f', 'max.f', 'corn.f', 'rice.f', 'clover.f', 'laurel.f', 
            'min.d', 'max.d', 'bamboo.d', 'lettuce.d', 'walnut.d', 'chestnut.d'),
  N = c(1, 3, 1.518, 2.275, 1.875, 2.660,
        1.5, 3.6, 2.698, 2.107, 2.656, 1.826),
  Cab = c(0, 100, 58, 23.7, 46.7, 74.1,
          0, 100, 70.8, 35.2, 62.8, 47.7),
  Cw = c(0.004, 0.04, 0.0131, 0.0075, 0.01, 0.0199,
         0.000063, 0.000900, 0.000117, 0.000244, 0.000263, 0.000307), 
  Cm = c(0.0019, 0.0165, 0.003662, 0.005811, 0.003014, 0.013520, 
         0.0019, 0.0165, 0.009327, 0.002250, 0.006573, 0.004305)
)

load("data/dataSpec_p4.RData")