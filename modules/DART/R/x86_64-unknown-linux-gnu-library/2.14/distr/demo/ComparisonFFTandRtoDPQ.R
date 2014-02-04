require(distr)
options("newDevice"=TRUE)
Pause <- function() { cat("Hit <enter> to continue..."); readline()}

#################################################
## Comparison 1 - Exact, FFT and RtoDPQ 
#################################################

N1 <- Norm(0,3)
N2 <- Norm(0,4)
rnew1 <- function(n) r(N1)(n) + r(N2)(n) 

#  calls exact formula -> N(0,5)
X <- N1 + N2 
# calls FFT
Y <- N1 + as(N2, "AbscontDistribution") 
# appoximation via RtoDPQ
Z <- new("AbscontDistribution", r = rnew1) 

# density-plot
x <- seq(-15,15,0.01)
plot(x, d(X)(x), type = "l", lwd = 3, xlab = "", ylab = "density",  
      main = "Comparison 1", col = "black")
lines(x, d(Y)(x), col = "yellow")
lines(x, d(Z)(x), col = "red")
legend("topleft",  legend = c("Exact", "FFT-Approximation", 
                                             "RtoDQP-Approximation"),
           fill = c("black", "yellow", "red"))
Pause()

#################################################
## Comparison 2 - "Exact" Formula and RtoDPQ
#################################################

B <- Binom(size = 6, prob = 0.5) * 10
N <- Norm()
rnew2 <- function(n) r(B)(n) + r(N)(n)

# calls "exact" fomula
Y <- B + N 
# appoximation via RtoDPQ
Z <- new("AbscontDistribution", r = rnew2) 

# density-plot
x  <- seq(-5,65,0.01)
plot(x, d(Y)(x), type = "l", xlab = "", ylab = "density",
      main = "Comparison 2", col = "black")
lines(x, d(Z)(x), col = "red")
legend("topleft", legend = c("Exact", "RtoDQP-Approximation"),
           fill = c("black", "red"))
